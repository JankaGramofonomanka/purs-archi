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

  let showPageActions = false;
  for (const actionBar of document.querySelectorAll("[data-page-actions]")) {
    if (!(actionBar instanceof HTMLElement)) {
      throw new Error("Invalid page action bar");
    }
    actionBar.hidden = actionBar.dataset.pageActions !== mode;
    showPageActions ||= !actionBar.hidden;
  }

  const demoPageElement = document.querySelector(".demo-page");
  if (!(demoPageElement instanceof HTMLElement)) {
    throw new Error("Missing .demo-page element");
  }
  demoPageElement.classList.toggle("has-page-actions", showPageActions);

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

const maxColumns = 20;
const minBayWidth = 1;
const maxBayWidth = 12;
// The motif of the columns' rythm: beats separated by `:`, each a `|` per column or `.` for none.
const rythmPattern = /^(\.|\|+)(:(\.|\|+))*$/;

// Renders the collonade form from `initial` and reports every edit to `onChange`. Each of
// `treatises` lists the keys of the orders it defines and of those it gives a pedestal; the
// form only offers those. Each storey of `orders` is `{ order, pedestal }`, the pedestal being
// a key of `pedestalNames`.
const setupCollonadeForm = (treatises, initial, onChange) => {
  const treatiseGroup = document.querySelector("#collonade-treatises");
  const orderList = document.querySelector("#collonade-orders");
  const addButton = document.querySelector("#collonade-add-order");
  const columnsInput = document.querySelector("#collonade-columns");
  const bayWidthInput = document.querySelector("#collonade-bay-width");
  const rythmInput = document.querySelector("#collonade-rythm");
  if (
    !(treatiseGroup instanceof HTMLElement) ||
    !(orderList instanceof HTMLOListElement) ||
    !(addButton instanceof HTMLButtonElement) ||
    !(columnsInput instanceof HTMLInputElement) ||
    !(bayWidthInput instanceof HTMLInputElement) ||
    !(rythmInput instanceof HTMLInputElement)
  ) {
    throw new Error("Missing collonade form controls");
  }

  const form = {
    treatise: initial.treatise,
    orders: initial.orders.map(storey => ({ ...storey })),
    numOfColumns: initial.numOfColumns,
    bayWidth: initial.bayWidth,
    rythm: initial.rythm
  };
  const report = () => onChange({
    treatise: form.treatise,
    orders: form.orders.map(storey => ({ ...storey })),
    numOfColumns: form.numOfColumns,
    bayWidth: form.bayWidth,
    rythm: form.rythm
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

  const render = () => {
    renderTreatises();
    renderOrders();
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
      report();
    }
  });
  bayWidthInput.addEventListener("change", () => {
    bayWidthInput.value = String(form.bayWidth);
  });

  rythmInput.value = form.rythm;
  rythmInput.addEventListener("input", () => {
    const value = rythmInput.value;
    if (rythmPattern.test(value) && value !== form.rythm) {
      form.rythm = value;
      report();
    }
  });
  rythmInput.addEventListener("change", () => {
    rythmInput.value = form.rythm;
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
