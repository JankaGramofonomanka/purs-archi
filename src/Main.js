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

// Icon of the arcade switch: an arch between two piers on the ground line.
const arcadeIcon = () => {
  const svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
  svg.setAttribute("viewBox", "0 0 24 20");
  svg.setAttribute("aria-hidden", "true");
  svg.innerHTML = '<path d="M2 2h20v15.5h-5V10a5 5 0 0 0-10 0v7.5H2zM1 17.5h22V19H1z"/>';
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
  beats: "Between axes",
  groups: "Between groups"
};

// How the arches above the lowest arcade, which sets the bay width, fill their wider bays; each
// keeps the height of its order's arch.
const arcadeModeNames = {
  arches: "Arches as the order",
  width: "Constant arch width",
  piers: "Piers as the order"
};
const arcadeModeTitles = {
  arches: "Every arch as wide as its order says; the piers widen upwards",
  width: "Every arch as wide as the lowest arcade's, as at the Colosseum",
  piers: "Every pier as wide as its order says; the arches widen upwards, as at the theatre of Marcellus"
};

// Bounds of the drawing's scale (lower diameter of the bottom columns, in pixels) and the factor
// each click of the zoom buttons multiplies or divides it by.
const minScale = 2;
const maxScale = 80;
const zoomStep = 1.25;

// Most columns a beat of the rythm can hold.
const maxColumnsPerBeat = 3;

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
// `treatises` lists the keys of the orders it defines, of those it gives a pedestal, and of those
// it gives an arcade without (`arcades`) and with (`pedestalArcades`) pedestals, and says whether
// it `stacks` orders (gives a rule for placing one over another); the form only offers those.
// Each storey of `orders` is `{ order, pedestal, arcade }`, the pedestal being a key of
// `pedestalNames` and `arcade` whether its columns frame arches; `arcadeMode` is a key of
// `arcadeModeNames`. `formStatus(form)` says whether any storey frames an arcade (`arcades`) and then
// the bay width the arches set (`bayWidth`), which replaces the one typed in, and for each storey
// whether its arches would rise higher than its columns (`archTooTall`), so it cannot frame one.
const setupCollonadeForm = (treatises, formStatus, initial, onChange) => {
  const treatiseGroup = document.querySelector("#collonade-treatises");
  const orderList = document.querySelector("#collonade-orders");
  const addButton = document.querySelector("#collonade-add-order");
  const addColumnButton = document.querySelector("#collonade-add-column");
  const addColumnLeftButton = document.querySelector("#collonade-add-column-left");
  const columnCountLabel = document.querySelector("#collonade-column-count");
  const bayCountLabel = document.querySelector("#collonade-bay-count");
  const bayWidthInput = document.querySelector("#collonade-bay-width");
  const windowWidthInput = document.querySelector("#collonade-window-width");
  const beatList = document.querySelector("#collonade-beats");
  const bayList = document.querySelector("#collonade-bays");
  const repeatBeatsButton = document.querySelector("#collonade-repeat-beats");
  const repeatBaysButton = document.querySelector("#collonade-repeat-bays");
  const mirrorButtons = {
    beats: {
      right: document.querySelector("#collonade-mirror-beats-right"),
      left: document.querySelector("#collonade-mirror-beats-left")
    },
    bays: {
      right: document.querySelector("#collonade-mirror-bays-right"),
      left: document.querySelector("#collonade-mirror-bays-left")
    }
  };
  const bayMeasureButton = document.querySelector("#collonade-bay-measure");
  const arcadeModeGroup = document.querySelector("#collonade-arcade-modes");
  const zoomInButton = document.querySelector("#collonade-zoom-in");
  const zoomOutButton = document.querySelector("#collonade-zoom-out");
  if (
    !(treatiseGroup instanceof HTMLElement) ||
    !(orderList instanceof HTMLOListElement) ||
    !(addButton instanceof HTMLButtonElement) ||
    !(addColumnButton instanceof HTMLButtonElement) ||
    !(addColumnLeftButton instanceof HTMLButtonElement) ||
    !(columnCountLabel instanceof HTMLElement) ||
    !(bayCountLabel instanceof HTMLElement) ||
    !(bayWidthInput instanceof HTMLInputElement) ||
    !(windowWidthInput instanceof HTMLInputElement) ||
    !(beatList instanceof HTMLOListElement) ||
    !(bayList instanceof HTMLOListElement) ||
    !(repeatBeatsButton instanceof HTMLButtonElement) ||
    !(repeatBaysButton instanceof HTMLButtonElement) ||
    !Object.values(mirrorButtons).every(({ right, left }) =>
      right instanceof HTMLButtonElement && left instanceof HTMLButtonElement) ||
    !(bayMeasureButton instanceof HTMLButtonElement) ||
    !(arcadeModeGroup instanceof HTMLElement) ||
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
    offset: { ...initial.offset },
    arcadeMode: initial.arcadeMode
  };
  const snapshot = () => ({
    treatise: form.treatise,
    orders: form.orders.map(storey => ({ ...storey })),
    numOfColumns: form.numOfColumns,
    bayWidth: form.bayWidth,
    bayMeasure: form.bayMeasure,
    rythm: [...form.rythm],
    bays: [...form.bays],
    windowWidth: form.windowWidth,
    scale: form.scale,
    offset: { ...form.offset },
    arcadeMode: form.arcadeMode
  });
  const report = () => onChange(snapshot())();
  const arcadeBay = () => formStatus(snapshot());
  // A storey whose arches would rise higher than its columns cannot frame an arcade. Turning one
  // off never changes the bay width (the lowest arcade's arches always fit), but repeat to be sure.
  const dropTooTallArcades = () => {
    for (let pass = 0; pass < form.orders.length; pass++) {
      const { archTooTall } = arcadeBay();
      const tooTall = form.orders.filter((storey, index) => storey.arcade && archTooTall[index]);
      if (tooTall.length === 0) return;
      tooTall.forEach(storey => { storey.arcade = false; });
    }
  };
  // The bay width the drawing uses: the arches' while any storey frames an arcade.
  const effectiveBayWidth = () => arcadeBay().bayWidth;
  const hasArcade = (treatise, { order, pedestal }) =>
    (pedestal === "none" ? treatise.arcades : treatise.pedestalArcades).includes(order);

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
      const missingArcades = form.orders
        .filter(storey => storey.arcade && !hasArcade(treatise, storey))
        .map(({ order }) => order);
      if (missingArcades.length > 0) {
        reasons.push(`gives no arcade for the ${names(missingArcades)} order as it stands`);
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
        storey.arcade &&= hasArcade(treatise, storey);
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
        storey.arcade &&= hasArcade(treatise, storey);
        render();
        report();
        orderList.querySelectorAll(".pedestal-toggle")[index]?.focus();
      });

      const arcadeButton = document.createElement("button");
      arcadeButton.type = "button";
      arcadeButton.className = "arcade-toggle";
      arcadeButton.append(arcadeIcon());
      arcadeButton.setAttribute("aria-pressed", String(storey.arcade));
      arcadeButton.setAttribute("aria-label", `Arcade of order ${index + 1} from the bottom`);
      const tooTall = arcadeBay().archTooTall[index] ?? false;
      arcadeButton.disabled = !hasArcade(treatise, storey) || tooTall;
      arcadeButton.title = !hasArcade(treatise, storey)
        ? `${treatise.name} gives no arcade for the ${orderNames[order]} order ${pedestal === "none" ? "without" : "with"} pedestals`
        : tooTall ? "The arch exceeds the column height"
        : storey.arcade ? "Arcade (click for columns only)" : "Columns only (click for an arcade)";
      arcadeButton.addEventListener("click", () => {
        storey.arcade = !storey.arcade;
        render();
        report();
        orderList.querySelectorAll(".arcade-toggle")[index]?.focus();
      });

      item.append(select, pedestalButton, arcadeButton, remove);
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

  const renderArcadeModes = () => {
    arcadeModeGroup.replaceChildren(...Object.entries(arcadeModeNames).map(([key, name]) => {
      const button = document.createElement("button");
      button.type = "button";
      button.className = "treatise-option";
      button.textContent = name;
      button.title = arcadeModeTitles[key];
      button.setAttribute("aria-pressed", String(key === form.arcadeMode));
      button.addEventListener("click", () => {
        form.arcadeMode = key;
        render();
        report();
      });
      return button;
    }));
  };

  // While a storey frames an arcade the bay width is the arches' and cannot be typed in; the
  // window width is kept within it.
  const renderBayWidth = () => {
    const { arcades, bayWidth } = arcadeBay();
    bayWidthInput.disabled = arcades;
    bayWidthInput.title = arcades ? "Set by the arches" : "";
    bayWidthInput.value = String(arcades ? Math.round(bayWidth * 1000) / 1000 : form.bayWidth);
    windowWidthInput.max = String(maxWindowWidth(bayWidth));
    if (form.windowWidth > maxWindowWidth(bayWidth)) {
      form.windowWidth = maxWindowWidth(bayWidth);
      windowWidthInput.value = String(form.windowWidth);
    }
  };
  bayMeasureButton.addEventListener("click", () => {
    form.bayMeasure = nextBayMeasure();
    render();
    report();
  });

  // Two sequences, left to right: a cell for each beat showing its columns, and below them, set between
  // each two beats, a cell for the bay showing what fills it. Clicking a beat steps to the next number of
  // columns (after the most a beat can hold, back to none); clicking a bay steps to the next fill. The `×`
  // under a beat removes it together with a bay next to it.
  const bayFills = Object.keys(bayFillNames);
  const sequenceCell = (list, icon, title, label, change, remove = null) => {
    const item = document.createElement("li");
    item.className = "sequence-cell";
    const strip = document.createElement("div");
    strip.className = "selection-strip";
    strip.title = "Drag along the strip to select cells to repeat";
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
    item.append(strip, toggle);
    if (remove !== null) {
      const removeButton = document.createElement("button");
      removeButton.type = "button";
      removeButton.className = "beat-remove";
      removeButton.textContent = "×";
      removeButton.disabled = remove.disabled;
      removeButton.setAttribute("aria-label", remove.label);
      removeButton.addEventListener("click", () => {
        const index = [...list.children].indexOf(item);
        remove.action();
        render();
        report();
        list.querySelectorAll(".beat-remove")[Math.min(index, list.children.length - 1)]?.focus();
      });
      item.append(removeButton);
    }
    return item;
  };
  const renderSequence = () => {
    beatList.replaceChildren(...form.rythm.map((columns, index) => {
      const next = (columns + 1) % (maxColumnsPerBeat + 1);
      return sequenceCell(
        beatList,
        beatIcon(columns),
        `${columnCount(columns)} (click for ${columnCount(next)})`,
        `Axis ${index + 1}: ${columnCount(columns)}, click for ${columnCount(next)}`,
        () => { form.rythm[index] = next; },
        { disabled: form.rythm.length === 1, label: `Remove axis ${index + 1}`, action: () => removeBeat(index) }
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

  // There is always a beat for each column and a bay between each two beats. A new column comes at the
  // right end with a single column on its beat and a window in the bay before it; removing a beat removes
  // the bay right of it too (left of it for the last beat).
  const addColumn = () => {
    form.rythm.push(1);
    form.bays.push("window");
    form.numOfColumns = form.rythm.length;
  };
  // The same at the left end, with the bay after the new column; the selections stay on their cells.
  const addColumnLeft = () => {
    form.rythm.unshift(1);
    form.bays.unshift("window");
    form.numOfColumns = form.rythm.length;
    for (const row of sequenceRows) {
      if (row.selection !== null) {
        row.selection = { anchor: row.selection.anchor + 1, focus: row.selection.focus + 1 };
      }
    }
  };
  const removeBeat = index => {
    const bay = Math.min(index, form.bays.length - 1);
    form.rythm.splice(index, 1);
    form.bays.splice(bay, 1);
    form.numOfColumns = form.rythm.length;
    removeFromSelection(sequenceRows[0], index);
    removeFromSelection(sequenceRows[1], bay);
  };
  // Keeps the selection of a sequence on the same cells when the cell at `index` is removed.
  const removeFromSelection = (row, index) => {
    const range = selectionRange(row.selection);
    if (range === null) {
      return;
    }
    const shift = i => i > index ? i - 1 : i;
    const start = shift(range.start);
    const end = range.end >= index ? range.end - 1 : range.end;
    row.selection = end < start ? null : { anchor: start, focus: end };
  };
  addColumnLeftButton.addEventListener("click", () => {
    addColumnLeft();
    render();
    report();
    sequenceRows[0].list.parentElement?.scrollTo({ left: 0 });
  });
  addColumnButton.addEventListener("click", () => {
    addColumn();
    render();
    report();
    sequenceRows[0].list.parentElement?.scrollTo({ left: Number.MAX_SAFE_INTEGER });
  });
  // How many axes (beats: places for a column or a group of them, filled or not) and bays there are.
  const renderColumnCount = () => {
    const axes = form.rythm.length;
    columnCountLabel.textContent = `${axes} ${axes === 1 ? "axis" : "axes"}`;
    const bays = form.bays.length;
    bayCountLabel.textContent = `${bays} ${bays === 1 ? "bay" : "bays"}`;
  };
  const renderAddColumn = () => {
    const full = form.rythm.length >= maxColumns;
    addColumnButton.disabled = full;
    addColumnLeftButton.disabled = full;
    addColumnButton.title = full
      ? `A collonade can have at most ${maxColumns} columns`
      : "Add axis to the right";
    addColumnLeftButton.title = full
      ? addColumnButton.title
      : "Add axis to the left";
  };

  // Each sequence has its own selection: a run of cells chosen by dragging along the strip next to them,
  // which `Repeat` lays over the whole sequence, keeping the selected cells where they are. Pressing a
  // strip of a cell that alone is selected clears the selection.
  const sequenceRows = [
    {
      list: beatList,
      button: repeatBeatsButton,
      mirror: mirrorButtons.beats,
      items: () => form.rythm,
      set: items => { form.rythm = items; },
      noun: "axes",
      selection: null
    },
    {
      list: bayList,
      button: repeatBaysButton,
      mirror: mirrorButtons.bays,
      items: () => form.bays,
      set: items => { form.bays = items; },
      noun: "bays",
      selection: null
    }
  ];
  const selectionRange = selection => selection === null
    ? null
    : { start: Math.min(selection.anchor, selection.focus), end: Math.max(selection.anchor, selection.focus) };
  const renderSelection = row => {
    const range = selectionRange(row.selection);
    [...row.list.children].forEach((cell, index) => {
      cell.classList.toggle("selected", range !== null && index >= range.start && index <= range.end);
    });
    const length = row.items().length;
    row.button.disabled = range === null || range.end - range.start + 1 >= length;
    row.button.title = range === null
      ? `Select ${row.noun} by dragging along the strip, then repeat them over the whole row`
      : `Repeat the ${range.end - range.start + 1} selected ${row.noun} over the whole row`;
  };
  // Makes the sequence symmetrical about its middle: `"right"` copies the left half mirrored onto the right
  // one, `"left"` the right half onto the left one; a cell in the very middle stays.
  const mirrorSequence = (row, onto) => {
    const items = row.items();
    const n = items.length;
    const kept = i => onto === "right" ? i < n / 2 : i >= (n - 1) / 2;
    row.set(items.map((item, i) => kept(i) ? item : items[n - 1 - i]));
    render();
    report();
  };
  const renderMirror = row => {
    const items = row.items();
    const symmetrical = items.every((item, i) => item === items[items.length - 1 - i]);
    for (const [onto, button] of Object.entries(row.mirror)) {
      const from = onto === "right" ? "left" : "right";
      button.disabled = symmetrical;
      button.title = symmetrical
        ? `The ${row.noun} are already symmetrical`
        : `Make the ${row.noun} symmetrical: copy the ${from} half, mirrored, onto the ${onto} half`;
    }
  };
  const repeatSelection = row => {
    const range = selectionRange(row.selection);
    if (range === null) {
      return;
    }
    const items = row.items();
    const segment = items.slice(range.start, range.end + 1);
    const at = i => ((i - range.start) % segment.length + segment.length) % segment.length;
    row.set(items.map((_, i) => segment[at(i)]));
    render();
    report();
  };

  let selectionDrag = null;
  const cellIndexAt = (row, x, y) => {
    const cell = document.elementFromPoint(x, y)?.closest(".sequence-cell");
    return cell instanceof HTMLElement && cell.parentElement === row.list
      ? [...row.list.children].indexOf(cell)
      : null;
  };
  for (const row of sequenceRows) {
    row.button.addEventListener("click", () => repeatSelection(row));
    row.mirror.right.addEventListener("click", () => mirrorSequence(row, "right"));
    row.mirror.left.addEventListener("click", () => mirrorSequence(row, "left"));
    row.list.addEventListener("pointerdown", event => {
      const strip = event.target instanceof Element ? event.target.closest(".selection-strip") : null;
      if (strip === null || event.button !== 0) {
        return;
      }
      event.preventDefault();
      const index = [...row.list.children].indexOf(strip.closest(".sequence-cell"));
      const range = selectionRange(row.selection);
      if (range !== null && range.start === index && range.end === index) {
        row.selection = null;
      } else {
        row.selection = { anchor: index, focus: index };
        selectionDrag = { row, pointerId: event.pointerId };
      }
      renderSelection(row);
    });
  }
  window.addEventListener("pointermove", event => {
    if (selectionDrag === null || event.pointerId !== selectionDrag.pointerId) {
      return;
    }
    const { row } = selectionDrag;
    const index = cellIndexAt(row, event.clientX, event.clientY);
    if (index !== null && index !== row.selection.focus) {
      row.selection.focus = index;
      renderSelection(row);
    }
  });
  const endSelectionDrag = event => {
    if (selectionDrag !== null && event.pointerId === selectionDrag.pointerId) {
      selectionDrag = null;
    }
  };
  window.addEventListener("pointerup", endSelectionDrag);
  window.addEventListener("pointercancel", endSelectionDrag);

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
    dropTooTallArcades();
    renderZoom();
    renderTreatises();
    renderOrders();
    renderAddButton();
    renderBayMeasures();
    renderArcadeModes();
    renderBayWidth();
    renderSequence();
    sequenceRows.forEach(renderSelection);
    sequenceRows.forEach(renderMirror);
    renderAddColumn();
    renderColumnCount();
  };

  addButton.addEventListener("click", () => {
    const available = currentTreatise().orders;
    const last = form.orders[form.orders.length - 1].order;
    const order = available.find(key => available.indexOf(key) > available.indexOf(last)) ?? last;
    form.orders.push({ order, pedestal: "none", arcade: false });
    render();
    report();
  });

  bayWidthInput.min = String(minBayWidth);
  bayWidthInput.max = String(maxBayWidth);
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
  windowWidthInput.value = String(form.windowWidth);
  windowWidthInput.addEventListener("input", () => {
    const value = Number(windowWidthInput.value);
    if (
      windowWidthInput.value !== "" &&
      Number.isFinite(value) &&
      value >= minWindowWidth &&
      value <= maxWindowWidth(effectiveBayWidth()) &&
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

export const setupRoutes = onRouteChange => onAction => treatises => formStatus => initialCollonade => onCollonadeChange => () => {
  setupCollonadeForm(treatises, formStatus, initialCollonade, onCollonadeChange);
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
