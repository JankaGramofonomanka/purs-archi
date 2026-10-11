const updateRoute = () => {
  const routeLabel = document.getElementById("route-label");
  if (!(routeLabel instanceof HTMLElement)) {
    throw new Error("Missing #route-label element");
  }

  const path = window.location.pathname.replace(/\/+$/, "") || "/";
  const routeLabels = {
    "/svg": "SVG",
    "/canvas": "Canvas",
    "/purescript-drawing": "PureScript Drawing"
  };
  const label = routeLabels[path];
  routeLabel.hidden = label === undefined;
  routeLabel.textContent = label ?? "";

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
