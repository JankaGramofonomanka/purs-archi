export const mountSvg = markup => () => {
  const target = document.getElementById("svg-target");
  if (!(target instanceof HTMLElement)) {
    throw new Error("Missing #svg-target element");
  }

  target.innerHTML = markup;
};

export const getSvgDimensions = () => {
  const target = document.getElementById("svg-target");
  if (!(target instanceof HTMLElement)) {
    throw new Error("Missing #svg-target element");
  }

  const { width, height } = target.getBoundingClientRect();
  if (width === 0 || height === 0) {
    throw new Error("#svg-target has no visible drawing area");
  }

  return { width, height };
};
