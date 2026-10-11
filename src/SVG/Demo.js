const svgNamespace = "http://www.w3.org/2000/svg";

const getSvgTarget = () => {
  const target = document.getElementById("svg-target");
  if (!(target instanceof SVGSVGElement)) {
    throw new Error("Missing #svg-target SVG element");
  }

  return target;
};

export const getSvgDimensions = () => {
  const target = getSvgTarget();
  const bounds = target.getBoundingClientRect();
  if (bounds.width === 0 || bounds.height === 0) {
    throw new Error("SVG drawing area has no visible size");
  }

  return { width: bounds.width, height: bounds.height };
};

export const mountSvg = width => height => elements => () => {
  const target = getSvgTarget();
  target.setAttribute("viewBox", `0 0 ${width} ${height}`);
  const nodes = elements.map(({ tag, attributes }) => {
    const node = document.createElementNS(svgNamespace, tag);
    for (const { name, value } of attributes) {
      node.setAttribute(name, value);
    }
    return node;
  });
  target.replaceChildren(...nodes);
};
