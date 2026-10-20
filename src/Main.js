// Each mode prefixes the backend routes ("" is the algorithm page); a mode may have its own action bar.
const modes = ["", "/demo", "/collonade"];
const backends = ["/svg", "/canvas", "/purescript-drawing"];

const updateRoute = onRouteChange => {
  const path = window.location.pathname.replace(/\/+$/, "") || "/";
  const routes = modes.flatMap(mode => backends.map(backend => mode + backend));
  const route = routes.includes(path) ? path : "/svg";
  if (path !== route) {
    window.history.replaceState({}, "", route);
  }
  currentRoute = route;
  const mode = getMode(route);
  const demoPage = getDemoPage(route);

  for (const page of document.querySelectorAll("[data-demo-page]")) {
    if (!(page instanceof HTMLElement)) {
      throw new Error("Invalid demo page");
    }
    page.hidden = page.dataset.demoPage !== demoPage;
  }

  // A mode's action bars sit on the left, or along the bottom if they are a `.bottom-bar`.
  let showPageActions = false;
  let showBottomBar = false;
  for (const actionBar of document.querySelectorAll("[data-page-actions]")) {
    if (!(actionBar instanceof HTMLElement)) {
      throw new Error("Invalid page action bar");
    }
    actionBar.hidden = actionBar.dataset.pageActions !== mode;
    if (actionBar.classList.contains("bottom-bar")) {
      showBottomBar ||= !actionBar.hidden;
    } else {
      showPageActions ||= !actionBar.hidden;
    }
  }

  const demoPageElement = document.querySelector(".demo-page");
  if (!(demoPageElement instanceof HTMLElement)) {
    throw new Error("Missing .demo-page element");
  }
  demoPageElement.classList.toggle("has-page-actions", showPageActions);
  demoPageElement.classList.toggle("has-bottom-bar", showBottomBar);

  resizeCanvasForRoute(route);
  const pageSurface = document.querySelector(
    `[data-demo-page="${demoPage}"] .demo-square`
  );
  if (!(pageSurface instanceof SVGSVGElement || pageSurface instanceof HTMLCanvasElement)) {
    throw new Error(`Missing drawing surface for ${route}`);
  }
  const surfaceBounds = pageSurface.getBoundingClientRect();
  const surfaceWidth = surfaceBounds.width;
  const surfaceHeight = surfaceBounds.height;
  if (surfaceWidth === 0 || surfaceHeight === 0) {
    throw new Error(`Drawing surface for ${route} has no visible area`);
  }

  const routeLinks = document.querySelectorAll(".route-link");
  routeLinks.forEach(link => {
    if (!(link instanceof HTMLAnchorElement)) {
      throw new Error("Invalid route link");
    }

    const pageRoute = link.dataset.page;
    if (pageRoute === undefined) {
      throw new Error("Route link is missing its page");
    }
    const linkRoute = mode + pageRoute;
    link.dataset.route = linkRoute;
    link.href = linkRoute;

    if (pageRoute === demoPage) {
      link.setAttribute("aria-current", "page");
    } else {
      link.removeAttribute("aria-current");
    }
  });

  onRouteChange(route)(surfaceWidth)(surfaceHeight)();
};

let currentRoute = "/";

const getMode = route => route.slice(0, route.lastIndexOf("/"));

const getDemoPage = route => route.slice(getMode(route).length);

const resizeCanvasForRoute = route => {
  const page = getDemoPage(route);
  const canvasIds = {
    "/canvas": "canvas-target",
    "/purescript-drawing": "drawing-target"
  };
  const canvasId = canvasIds[page];
  if (canvasId === undefined) {
    return;
  }

  const canvas = document.getElementById(canvasId);
  if (!(canvas instanceof HTMLCanvasElement)) {
    throw new Error(`Missing #${canvasId} canvas`);
  }

  const bounds = canvas.getBoundingClientRect();
  const width = Math.round(bounds.width);
  const height = Math.round(bounds.height);
  if (width === 0 || height === 0) {
    throw new Error(`Canvas #${canvasId} has no visible drawing area`);
  }

  if (canvas.width !== width || canvas.height !== height) {
    canvas.width = width;
    canvas.height = height;
  }
};

const orderNames = {
  tuscan: "Tuscan",
  doric: "Doric",
  ionic: "Ionic",
  corynthian: "Corynthian",
  composite: "Composite"
};

const pedestalNames = {
  none: "No pedestal",
  pedestals: "Pedestals",
  podium: "Podium"
};

// Icons of the pedestal modes: a column on the ground line, columns each on a pedestal, and
// columns on one continuous podium.
const pedestalIcons = {
  none: '<path d="M5 2h2.5v15.5H5zM16.5 2H19v15.5h-2.5zM1 17.5h22V19H1z"/>',
  pedestals: '<path d="M5 2h2.5v9H5zM16.5 2H19v9h-2.5zM2.5 11h7.5v6.5H2.5zM14 11h7.5v6.5H14zM1 17.5h22V19H1z"/>',
  podium: '<path d="M5 2h2.5v9H5zM16.5 2H19v9h-2.5zM2.5 11h19v6.5h-19zM1 17.5h22V19H1z"/>'
};

const pedestalIcon = mode => {
  const svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
  svg.setAttribute("viewBox", "0 0 24 20");
  svg.setAttribute("aria-hidden", "true");
  svg.innerHTML = pedestalIcons[mode];
  return svg;
};

const maxColumns = 40;
const minBayWidth = 1;
const maxBayWidth = 12;
// Bounds of the width of the windows' openings, in lower diameters of the bottom columns: the opening
// and its architrave (a sixth of the opening on each side) at most as wide as the bay width less the
// lower diameter of the bottom columns.
const minWindowWidth = 0;
const maxWindowWidth = bayWidth => Math.max(minWindowWidth, (bayWidth - 1) * 3 / 4);
// What the bay width measures: the distance between the centerlines of neighbouring beats, or
// between the facing outer columns of neighbouring groups of columns.
const bayMeasureNames = {
  beats: "Between beats",
  groups: "Between groups"
};

// Bounds of the drawing's scale (lower diameter of the bottom columns, in pixels) and the factor
// each click of the zoom buttons multiplies or divides it by.
const minScale = 2;
const maxScale = 80;
const zoomStep = 1.25;

// Most columns a beat of the rythm can hold.
const maxColumnsPerBeat = 6;

// Icon of a beat of the rythm: its columns on a ground line.
const beatIcon = columns => {
  const svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
  svg.setAttribute("viewBox", "0 0 32 24");
  svg.setAttribute("aria-hidden", "true");
  const columnWidth = 2.5;
  const spacing = 5;
  const left = 16 - ((columns - 1) * spacing + columnWidth) / 2;
  const shafts = Array.from({ length: columns }, (_, i) => `M${left + i * spacing} 3h${columnWidth}v18.5h-${columnWidth}z`);
  svg.innerHTML = `<path d="${shafts.join("")}M1 21.5h30V23H1z"/>`;
  return svg;
};

// What can fill a bay between two beats, in the order clicking a bay steps through them.
const bayFillNames = {
  wall: "Wall",
  window: "Window"
};

// Icon of a bay: a plain wall, or a framed window in it.
const bayIcon = fill => {
  const svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
  svg.setAttribute("viewBox", "0 0 32 24");
  svg.setAttribute("aria-hidden", "true");
  const wall = "M3 1.5h26v21H3z";
  const window = "M11 4h10v15H11zM12.5 5.5v12h7v-12z";
  svg.innerHTML = fill === "window"
    ? `<path d="${wall}" fill="none" stroke="currentColor" stroke-width="1"/><path d="${window}" fill-rule="evenodd"/>`
    : `<path d="${wall}" fill="none" stroke="currentColor" stroke-width="1"/>`;
  return svg;
};

const columnCount = n => n === 0 ? "no column" : n === 1 ? "1 column" : `${n} columns`;

// Renders the collonade form from `initial` and reports every edit to `onChange`. Each of
// `treatises` lists the keys of the orders it defines and of those it gives a pedestal, and says
// whether it `stacks` orders (gives a rule for placing one over another); the form only offers those. Each storey of `orders` is `{ order, pedestal }`, the pedestal being
// a key of `pedestalNames`.
const setupCollonadeForm = (treatises, initial, onChange) => {
  const treatiseGroup = document.querySelector("#collonade-treatises");
  const orderList = document.querySelector("#collonade-orders");
  const addButton = document.querySelector("#collonade-add-order");
  const columnsInput = document.querySelector("#collonade-columns");
  const bayWidthInput = document.querySelector("#collonade-bay-width");
  const windowWidthInput = document.querySelector("#collonade-window-width");
  const beatList = document.querySelector("#collonade-beats");
  const bayList = document.querySelector("#collonade-bays");
  const bayMeasureButton = document.querySelector("#collonade-bay-measure");
  const zoomInButton = document.querySelector("#collonade-zoom-in");
  const zoomOutButton = document.querySelector("#collonade-zoom-out");
  if (
    !(treatiseGroup instanceof HTMLElement) ||
    !(orderList instanceof HTMLOListElement) ||
    !(addButton instanceof HTMLButtonElement) ||
    !(columnsInput instanceof HTMLInputElement) ||
    !(bayWidthInput instanceof HTMLInputElement) ||
    !(windowWidthInput instanceof HTMLInputElement) ||
    !(beatList instanceof HTMLOListElement) ||
    !(bayList instanceof HTMLOListElement) ||
    !(bayMeasureButton instanceof HTMLButtonElement) ||
    !(zoomInButton instanceof HTMLButtonElement) ||
    !(zoomOutButton instanceof HTMLButtonElement)
  ) {
    throw new Error("Missing collonade form controls");
  }

  const form = {
    treatise: initial.treatise,
    orders: initial.orders.map(storey => ({ ...storey })),
    numOfColumns: initial.numOfColumns,
    bayWidth: initial.bayWidth,
    bayMeasure: initial.bayMeasure,
    rythm: [...initial.rythm],
    bays: [...initial.bays],
    windowWidth: initial.windowWidth,
    scale: initial.scale,
    offset: { ...initial.offset }
  };
  const report = () => onChange({
    treatise: form.treatise,
    orders: form.orders.map(storey => ({ ...storey })),
    numOfColumns: form.numOfColumns,
    bayWidth: form.bayWidth,
    bayMeasure: form.bayMeasure,
    rythm: [...form.rythm],
    bays: [...form.bays],
    windowWidth: form.windowWidth,
    scale: form.scale,
    offset: { ...form.offset }
  })();

  const currentTreatise = () => {
    const treatise = treatises.find(({ key }) => key === form.treatise);
    if (treatise === undefined) {
      throw new Error(`Unknown treatise: ${form.treatise}`);
    }
    return treatise;
  };

  const renderTreatises = () => {
    treatiseGroup.replaceChildren(...treatises.map(treatise => {
      const button = document.createElement("button");
      button.type = "button";
      button.className = "treatise-option";
      button.textContent = treatise.name;
      button.setAttribute("aria-pressed", String(treatise.key === form.treatise));
      const missingOrders = form.orders
        .filter(({ order }) => !treatise.orders.includes(order))
        .map(({ order }) => order);
      const missingPedestals = form.orders
        .filter(({ order, pedestal }) => pedestal !== "none" && !treatise.pedestals.includes(order))
        .map(({ order }) => order);
      const names = keys => [...new Set(keys)].map(order => orderNames[order]).join(", ");
      const reasons = [];
      if (missingOrders.length > 0) {
        reasons.push(`does not define the ${names(missingOrders)} order`);
      }
      if (missingPedestals.length > 0) {
        reasons.push(`gives no pedestal for the ${names(missingPedestals)} order`);
      }
      if (form.orders.length > 1 && !treatise.stacks) {
        reasons.push("gives no rule for stacking orders");
      }
      button.disabled = reasons.length > 0;
      if (button.disabled) {
        button.title = `${treatise.name} ${reasons.join(" and ")}`;
      }
      button.addEventListener("click", () => {
        form.treatise = treatise.key;
        render();
        report();
      });
      return button;
    }));
  };

  const renderOrders = () => {
    const treatise = currentTreatise();
    const available = treatise.orders;
    orderList.replaceChildren(...form.orders.map((storey, index) => {
      const { order, pedestal } = storey;
      const item = document.createElement("li");
      item.className = "order-row";

      const select = document.createElement("select");
      select.setAttribute("aria-label", `Order ${index + 1} from the bottom`);
      for (const [key, name] of Object.entries(orderNames)) {
        const option = new Option(name, key, false, key === order);
        option.disabled = !available.includes(key);
        select.append(option);
      }
      select.addEventListener("change", () => {
        storey.order = select.value;
        if (!treatise.pedestals.includes(storey.order)) {
          storey.pedestal = "none";
        }
        render();
        report();
      });

      const remove = document.createElement("button");
      remove.type = "button";
      remove.className = "order-remove";
      remove.textContent = "×";
      remove.setAttribute("aria-label", `Remove order ${index + 1}`);
      remove.disabled = form.orders.length === 1;
      remove.addEventListener("click", () => {
        form.orders.splice(index, 1);
        render();
        report();
      });

      const pedestalModes = Object.keys(pedestalNames);
      const pedestalButton = document.createElement("button");
      pedestalButton.type = "button";
      pedestalButton.className = "pedestal-toggle";
      pedestalButton.append(pedestalIcon(pedestal));
      pedestalButton.disabled = !treatise.pedestals.includes(order);
      if (pedestalButton.disabled) {
        pedestalButton.title = `${treatise.name} gives no pedestal for the ${orderNames[order]} order`;
      } else {
        const next = pedestalModes[(pedestalModes.indexOf(pedestal) + 1) % pedestalModes.length];
        pedestalButton.title = `${pedestalNames[pedestal]} (click for ${pedestalNames[next].toLowerCase()})`;
      }
      pedestalButton.setAttribute(
        "aria-label",
        `Pedestal of order ${index + 1} from the bottom: ${pedestalNames[pedestal]}`
      );
      pedestalButton.addEventListener("click", () => {
        storey.pedestal = pedestalModes[(pedestalModes.indexOf(storey.pedestal) + 1) % pedestalModes.length];
        render();
        report();
        orderList.querySelectorAll(".pedestal-toggle")[index]?.focus();
      });

      item.append(select, pedestalButton, remove);
      return item;
    }));
  };

  const bayMeasureKeys = Object.keys(bayMeasureNames);
  const nextBayMeasure = () =>
    bayMeasureKeys[(bayMeasureKeys.indexOf(form.bayMeasure) + 1) % bayMeasureKeys.length];
  const renderBayMeasures = () => {
    bayMeasureButton.textContent = bayMeasureNames[form.bayMeasure];
    bayMeasureButton.title = `Click for ${bayMeasureNames[nextBayMeasure()].toLowerCase()}`;
  };
  bayMeasureButton.addEventListener("click", () => {
    form.bayMeasure = nextBayMeasure();
    render();
    report();
  });

  // Two sequences, left to right: a cell for each beat showing its columns, and below them, set between
  // each two beats, a cell for the bay showing what fills it. Clicking a beat steps to the next number of
  // columns (after the most a beat can hold, back to none); clicking a bay steps to the next fill.
  const bayFills = Object.keys(bayFillNames);
  const sequenceCell = (list, icon, title, label, change) => {
    const item = document.createElement("li");
    item.className = "sequence-cell";
    const toggle = document.createElement("button");
    toggle.type = "button";
    toggle.className = "sequence-toggle";
    toggle.append(icon);
    toggle.title = title;
    toggle.setAttribute("aria-label", label);
    toggle.addEventListener("click", () => {
      const index = [...list.children].indexOf(item);
      change();
      render();
      report();
      list.querySelectorAll(".sequence-toggle")[index]?.focus();
    });
    item.append(toggle);
    return item;
  };
  const renderSequence = () => {
    beatList.replaceChildren(...form.rythm.map((columns, index) => {
      const next = (columns + 1) % (maxColumnsPerBeat + 1);
      return sequenceCell(
        beatList,
        beatIcon(columns),
        `${columnCount(columns)} (click for ${columnCount(next)})`,
        `Beat ${index + 1}: ${columnCount(columns)}, click for ${columnCount(next)}`,
        () => { form.rythm[index] = next; }
      );
    }));
    bayList.replaceChildren(...form.bays.map((fill, index) => {
      const next = bayFills[(bayFills.indexOf(fill) + 1) % bayFills.length];
      return sequenceCell(
        bayList,
        bayIcon(fill),
        `${bayFillNames[fill]} (click for ${bayFillNames[next].toLowerCase()})`,
        `Bay ${index + 1}: ${bayFillNames[fill].toLowerCase()}, click for ${bayFillNames[next].toLowerCase()}`,
        () => { form.bays[index] = next; }
      );
    }));
  };

  // Keeps a beat for each column and a bay between each two beats: new beats get a single column and
  // new bays a window, and the ones beyond the last column are dropped.
  const resizeSequence = numOfColumns => {
    const resize = (items, length, filler) =>
      Array.from({ length }, (_, i) => i < items.length ? items[i] : filler);
    form.rythm = resize(form.rythm, numOfColumns, 1);
    form.bays = resize(form.bays, numOfColumns - 1, "window");
  };

  const renderAddButton = () => {
    const treatise = currentTreatise();
    addButton.disabled = !treatise.stacks;
    if (addButton.disabled) {
      addButton.title = `${treatise.name} gives no rule for stacking orders`;
    } else {
      addButton.removeAttribute("title");
    }
  };

  const renderZoom = () => {
    zoomInButton.disabled = form.scale * zoomStep > maxScale;
    zoomOutButton.disabled = form.scale / zoomStep < minScale;
  };
  // Zooming keeps the point under `anchor` where it is: `anchor` is measured from the center of the
  // surface, like the offset, and is the center itself unless given.
  const zoom = (factor, anchor = { x: 0, y: 0 }) => {
    form.scale *= factor;
    form.offset = {
      x: anchor.x + (form.offset.x - anchor.x) * factor,
      y: anchor.y + (form.offset.y - anchor.y) * factor
    };
    renderZoom();
    report();
  };
  zoomInButton.addEventListener("click", () => zoom(zoomStep));
  zoomOutButton.addEventListener("click", () => zoom(1 / zoomStep));

  // Scrolling over the drawing zooms it while the zoom buttons are shown, one step per notch:
  // up enlarges, down shrinks, stopping at the same bounds as the buttons. The point under the cursor
  // stays put.
  const drawingShown = () => zoomInButton.closest("[hidden]") === null;
  for (const stage of document.querySelectorAll(".demo-stage")) {
    stage.addEventListener("wheel", event => {
      if (!drawingShown() || event.deltaY === 0) {
        return;
      }
      event.preventDefault();
      const button = event.deltaY < 0 ? zoomInButton : zoomOutButton;
      const surface = stage.querySelector(".demo-square");
      if (!button.disabled && surface instanceof Element) {
        const bounds = surface.getBoundingClientRect();
        zoom(event.deltaY < 0 ? zoomStep : 1 / zoomStep, {
          x: event.clientX - (bounds.left + bounds.width / 2),
          y: event.clientY - (bounds.top + bounds.height / 2)
        });
      }
    }, { passive: false });

    // Dragging with the main button moves the drawing along with the pointer.
    let drag = null;
    stage.addEventListener("pointerdown", event => {
      if (!drawingShown() || event.button !== 0) {
        return;
      }
      event.preventDefault();
      stage.setPointerCapture(event.pointerId);
      drag = { pointerId: event.pointerId, x: event.clientX, y: event.clientY };
      stage.classList.add("dragging");
    });
    stage.addEventListener("pointermove", event => {
      if (drag === null || event.pointerId !== drag.pointerId) {
        return;
      }
      const dx = event.clientX - drag.x;
      const dy = event.clientY - drag.y;
      if (dx === 0 && dy === 0) {
        return;
      }
      drag.x = event.clientX;
      drag.y = event.clientY;
      form.offset = { x: form.offset.x + dx, y: form.offset.y + dy };
      report();
    });
    const endDrag = event => {
      if (drag !== null && event.pointerId === drag.pointerId) {
        drag = null;
        stage.classList.remove("dragging");
      }
    };
    stage.addEventListener("pointerup", endDrag);
    stage.addEventListener("pointercancel", endDrag);
  }

  const render = () => {
    renderZoom();
    renderTreatises();
    renderOrders();
    renderAddButton();
    renderBayMeasures();
    renderSequence();
  };

  addButton.addEventListener("click", () => {
    const available = currentTreatise().orders;
    const last = form.orders[form.orders.length - 1].order;
    const order = available.find(key => available.indexOf(key) > available.indexOf(last)) ?? last;
    form.orders.push({ order, pedestal: "none" });
    render();
    report();
  });

  columnsInput.min = "1";
  columnsInput.max = String(maxColumns);
  columnsInput.value = String(form.numOfColumns);
  columnsInput.addEventListener("input", () => {
    const value = Number(columnsInput.value);
    if (Number.isInteger(value) && value >= 1 && value <= maxColumns && value !== form.numOfColumns) {
      form.numOfColumns = value;
      resizeSequence(value);
      render();
      report();
    }
  });
  columnsInput.addEventListener("change", () => {
    columnsInput.value = String(form.numOfColumns);
  });

  bayWidthInput.min = String(minBayWidth);
  bayWidthInput.max = String(maxBayWidth);
  bayWidthInput.value = String(form.bayWidth);
  bayWidthInput.addEventListener("input", () => {
    const value = Number(bayWidthInput.value);
    if (
      bayWidthInput.value !== "" &&
      Number.isFinite(value) &&
      value >= minBayWidth &&
      value <= maxBayWidth &&
      value !== form.bayWidth
    ) {
      form.bayWidth = value;
      windowWidthInput.max = String(maxWindowWidth(form.bayWidth));
      if (form.windowWidth > maxWindowWidth(form.bayWidth)) {
        form.windowWidth = maxWindowWidth(form.bayWidth);
        windowWidthInput.value = String(form.windowWidth);
      }
      report();
    }
  });
  bayWidthInput.addEventListener("change", () => {
    bayWidthInput.value = String(form.bayWidth);
  });

  windowWidthInput.min = String(minWindowWidth);
  windowWidthInput.max = String(maxWindowWidth(form.bayWidth));
  windowWidthInput.value = String(form.windowWidth);
  windowWidthInput.addEventListener("input", () => {
    const value = Number(windowWidthInput.value);
    if (
      windowWidthInput.value !== "" &&
      Number.isFinite(value) &&
      value >= minWindowWidth &&
      value <= maxWindowWidth(form.bayWidth) &&
      value !== form.windowWidth
    ) {
      form.windowWidth = value;
      report();
    }
  });
  windowWidthInput.addEventListener("change", () => {
    windowWidthInput.value = String(form.windowWidth);
  });

  render();
};

export const setupRoutes = onRouteChange => onAction => treatises => initialCollonade => onCollonadeChange => () => {
  setupCollonadeForm(treatises, initialCollonade, onCollonadeChange);
  updateRoute(onRouteChange);
  window.addEventListener("popstate", () => updateRoute(onRouteChange));
  window.addEventListener("resize", () => updateRoute(onRouteChange));

  const routeNav = document.querySelector(".route-nav");
  if (!(routeNav instanceof HTMLElement)) {
    throw new Error("Missing .route-nav element");
  }

  const stepButton = document.querySelector("#step-algorithm");
  if (!(stepButton instanceof HTMLButtonElement)) {
    throw new Error("Missing #step-algorithm button");
  }
  stepButton.addEventListener("click", () => onAction());

  routeNav.addEventListener("click", event => {
    if (
      !(event.target instanceof Element) ||
      event.defaultPrevented ||
      event.button !== 0 ||
      event.metaKey ||
      event.ctrlKey ||
      event.shiftKey ||
      event.altKey
    ) {
      return;
    }

    const link = event.target.closest("a[data-route]");
    if (
      !(link instanceof HTMLAnchorElement) ||
      link.target !== "" ||
      link.hasAttribute("download") ||
      link.origin !== window.location.origin
    ) {
      return;
    }

    event.preventDefault();
    window.history.pushState({}, "", link.href);
    updateRoute(onRouteChange);
  });
};
