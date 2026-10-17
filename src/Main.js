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
  doric: "Doric",
  ionic: "Ionic",
  corynthian: "Corynthian"
};

const maxColumns = 20;

// Renders the collonade form from `initial` and reports every edit to `onChange`.
const setupCollonadeForm = (initial, onChange) => {
  const orderList = document.querySelector("#collonade-orders");
  const addButton = document.querySelector("#collonade-add-order");
  const columnsInput = document.querySelector("#collonade-columns");
  if (
    !(orderList instanceof HTMLOListElement) ||
    !(addButton instanceof HTMLButtonElement) ||
    !(columnsInput instanceof HTMLInputElement)
  ) {
    throw new Error("Missing collonade form controls");
  }

  const form = { orders: [...initial.orders], numOfColumns: initial.numOfColumns };
  const report = () => onChange({ orders: [...form.orders], numOfColumns: form.numOfColumns })();

  const renderOrders = () => {
    orderList.replaceChildren(...form.orders.map((order, index) => {
      const item = document.createElement("li");
      item.className = "order-row";

      const select = document.createElement("select");
      select.setAttribute("aria-label", `Order ${index + 1} from the bottom`);
      for (const [key, name] of Object.entries(orderNames)) {
        select.append(new Option(name, key, false, key === order));
      }
      select.addEventListener("change", () => {
        form.orders[index] = select.value;
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
        renderOrders();
        report();
      });

      item.append(select, remove);
      return item;
    }));
  };

  addButton.addEventListener("click", () => {
    const keys = Object.keys(orderNames);
    const last = form.orders[form.orders.length - 1];
    form.orders.push(keys[Math.min(keys.indexOf(last) + 1, keys.length - 1)]);
    renderOrders();
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

  renderOrders();
};

export const setupRoutes = onRouteChange => onAction => initialCollonade => onCollonadeChange => () => {
  setupCollonadeForm(initialCollonade, onCollonadeChange);
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
