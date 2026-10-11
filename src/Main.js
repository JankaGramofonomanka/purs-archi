const updateRoute = onRouteChange => {
  const routeLabel = document.getElementById("route-label-demo");
  if (!(routeLabel instanceof HTMLElement)) {
    throw new Error("Missing #route-label-demo element");
  }

  const path = window.location.pathname.replace(/\/+$/, "") || "/";
  const routeLabels = {
    "/svg": "SVG",
    "/canvas": "Canvas",
    "/purescript-drawing": "PureScript Drawing"
  };
  const label = routeLabels[path];
  const route = label === undefined ? "/" : path;
  routeLabel.hidden = label === undefined;
  routeLabel.textContent = label ?? "";

  const homePage = document.getElementById("home-page");
  const demoPage = document.getElementById("demo-page");
  if (!(homePage instanceof HTMLElement) || !(demoPage instanceof HTMLElement)) {
    throw new Error("Missing page container");
  }
  homePage.hidden = route !== "/";
  demoPage.hidden = route === "/";

  for (const page of document.querySelectorAll("[data-demo-page]")) {
    if (!(page instanceof HTMLElement)) {
      throw new Error("Invalid demo page");
    }
    page.hidden = page.dataset.demoPage !== route;
  }

  const routeLinks = document.querySelectorAll(".route-link");
  routeLinks.forEach(link => {
    if (!(link instanceof HTMLAnchorElement)) {
      throw new Error("Invalid route link");
    }

    if (link.dataset.route === route) {
      link.setAttribute("aria-current", "page");
    } else {
      link.removeAttribute("aria-current");
    }
  });

  onRouteChange(route)();
};

export const setupRoutes = onRouteChange => () => {
  updateRoute(onRouteChange);
  window.addEventListener("popstate", () => updateRoute(onRouteChange));

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
    updateRoute(onRouteChange);
  });
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
