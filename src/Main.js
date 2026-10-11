const updateRoute = () => {
  const routeLabel = document.getElementById("route-label");
  if (!(routeLabel instanceof HTMLElement)) {
    throw new Error("Missing #route-label element");
  }

  const path = window.location.pathname.replace(/\/+$/, "") || "/";
  routeLabel.hidden = path !== "/test";
};

export const setupRoutes = () => {
  updateRoute();
  window.addEventListener("popstate", updateRoute);
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
