export const mountSvg = markup => () => {
  const target = document.getElementById("svg-target");
  if (!(target instanceof HTMLElement)) {
    throw new Error("Missing #svg-target element");
  }

  target.innerHTML = markup;
};
