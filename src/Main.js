const updateRoute = () => {
  const routeLabel = document.getElementById("route-label");
  if (!(routeLabel instanceof HTMLElement)) {
    throw new Error("Missing #route-label element");
  }

  const path = window.location.pathname.replace(/\/+$/, "") || "/";
  const routes = {
    "/svg": { label: "SVG", pageId: "svg-page" },
    "/canvas": { label: "Canvas", pageId: "canvas-page" },
    "/purescript-drawing": {
      label: "PureScript Drawing",
      pageId: "purescript-drawing-page"
    }
  };
  const route = routes[path];
  routeLabel.hidden = route === undefined;
  routeLabel.textContent = route?.label ?? "";

  const homePage = document.getElementById("home-page");
  const demoPage = document.getElementById("demo-page");
  if (!(homePage instanceof HTMLElement) || !(demoPage instanceof HTMLElement)) {
    throw new Error("Missing page container");
  }

  homePage.hidden = route !== undefined;
  demoPage.hidden = route === undefined;

  document.querySelectorAll(".demo-stage").forEach(stage => {
    if (!(stage instanceof HTMLElement)) {
      throw new Error("Invalid demo stage");
    }

    stage.hidden = stage.id !== route?.pageId;
  });

  const routeLinks = document.querySelectorAll(".route-link");
  routeLinks.forEach(link => {
    if (!(link instanceof HTMLAnchorElement)) {
      throw new Error("Invalid route link");
    }

    if (link.dataset.route === path) {
      link.setAttribute("aria-current", "page");
    } else {
      link.removeAttribute("aria-current");
    }
  });
};

export const setupRoutes = () => {
  updateRoute();
  window.addEventListener("popstate", updateRoute);

  const routeNav = document.querySelector(".route-nav");
  if (!(routeNav instanceof HTMLElement)) {
    throw new Error("Missing .route-nav element");
  }

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
    updateRoute();
  });
};

export const drawCanvasSquare = () => {
  const canvas = document.getElementById("canvas-square");
  if (!(canvas instanceof HTMLCanvasElement)) {
    throw new Error("Missing #canvas-square canvas");
  }

  const context = canvas.getContext("2d");
  if (context === null) {
    throw new Error("Unable to get 2D context for #canvas-square");
  }

  context.fillStyle = "#5268d8";
  context.fillRect(60, 60, 120, 120);
};

export const setCount = count => () => {
  const output = document.getElementById("count");
  if (!(output instanceof HTMLElement)) {
    throw new Error("Missing #count element");
  }

  output.textContent = String(count);
};

export const onIncrement = handler => () => {
  const button = document.getElementById("increment");
  if (!(button instanceof HTMLButtonElement)) {
    throw new Error("Missing #increment button");
  }

  button.addEventListener("click", handler);
};
