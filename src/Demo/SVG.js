const svgNamespace = "http://www.w3.org/2000/svg";

export const mountSvg = elements => () => {
  const target = document.getElementById("svg-target");
  if (!(target instanceof SVGSVGElement)) {
    throw new Error("Missing #svg-target SVG element");
  }

  const nodes = elements.map(({ tag, attributes }) => {
    const node = document.createElementNS(svgNamespace, tag);
    for (const { name, value } of attributes) {
      node.setAttribute(name, value);
    }
    return node;
  });
  target.replaceChildren(...nodes);
};
