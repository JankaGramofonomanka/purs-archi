const namespace = "http://www.w3.org/2000/svg";

const target = () => {
  const element = document.getElementById("svg-target");
  if (!(element instanceof SVGSVGElement)) {
    throw new Error("Missing #svg-target SVG element");
  }
  return element;
};

export const prepareSvg = () => {
  const element = target();
  const bounds = element.getBoundingClientRect();
  if (bounds.width === 0 || bounds.height === 0) {
    throw new Error("SVG drawing area has no visible size");
  }

  element.setAttribute("viewBox", `0 0 ${bounds.width} ${bounds.height}`);
  element.replaceChildren();
  return { width: bounds.width, height: bounds.height };
};

const append = (tag, attributes) => {
  const element = document.createElementNS(namespace, tag);
  for (const [name, value] of attributes) {
    element.setAttribute(name, String(value));
  }
  target().append(element);
};

export const appendRectangle = x => y => width => height => opacity => fill => () =>
  append("rect", [
    ["x", x], ["y", y], ["width", width], ["height", height],
    ["fill-opacity", opacity], ["fill", fill]
  ]);

export const appendPolygon = points => hasFill => fillOpacity => fill => hasContour => strokeWidth => strokeOpacity => strokeDash => strokeCap => stroke => () => {
  if (points.length < 3) return;

  const commands = points.map(({ x, y }, index) =>
    `${index === 0 ? "M" : "L"} ${x} ${y}`
  );
  const attributes = [["d", `${commands.join(" ")} Z`]];
  if (hasFill) {
    attributes.push(["fill-opacity", fillOpacity], ["fill", fill]);
  } else {
    attributes.push(["fill", "none"]);
  }
  if (hasContour) {
    attributes.push(
      ["stroke", stroke],
      ["stroke-width", strokeWidth],
      ["stroke-opacity", strokeOpacity],
      ["stroke-linecap", strokeCap]
    );
    if (strokeDash !== "") attributes.push(["stroke-dasharray", strokeDash]);
  }
  append("path", attributes);
};

export const appendStyledCircle = cx => cy => radius => hasFill => fillOpacity => fill => hasContour => strokeWidth => strokeOpacity => strokeDash => strokeCap => stroke => () => {
  const attributes = [["cx", cx], ["cy", cy], ["r", radius]];
  if (hasFill) {
    attributes.push(["fill-opacity", fillOpacity], ["fill", fill]);
  } else {
    attributes.push(["fill", "none"]);
  }
  if (hasContour) {
    attributes.push(
      ["stroke", stroke],
      ["stroke-width", strokeWidth],
      ["stroke-opacity", strokeOpacity],
      ["stroke-linecap", strokeCap]
    );
    if (strokeDash !== "") attributes.push(["stroke-dasharray", strokeDash]);
  }
  append("circle", attributes);
};

export const appendArc = cx => cy => radius => startAngle => endAngle => strokeWidth => strokeOpacity => strokeDash => strokeCap => stroke => () => {
  const span = endAngle - startAngle;
  const steps = Math.max(1, Math.ceil(Math.abs(span) / (2 * Math.PI) * 64));
  const points = Array.from({ length: steps + 1 }, (_, index) => {
    const angle = startAngle + span * index / steps;
    return `${cx + radius * Math.cos(angle)} ${cy + radius * Math.sin(angle)}`;
  });
  const attributes = [
    ["d", `M ${points[0]} ${points.slice(1).map(point => `L ${point}`).join(" ")}`],
    ["fill", "none"],
    ["stroke", stroke],
    ["stroke-width", strokeWidth],
    ["stroke-opacity", strokeOpacity],
    ["stroke-linecap", strokeCap]
  ];
  if (strokeDash !== "") attributes.push(["stroke-dasharray", strokeDash]);
  append("path", attributes);
};

export const appendLine = x1 => y1 => x2 => y2 => width => opacity => dash => cap => stroke => () => {
  const attributes = [
    ["x1", x1], ["y1", y1], ["x2", x2], ["y2", y2],
    ["stroke-width", width], ["stroke-opacity", opacity], ["stroke", stroke]
  ];
  if (dash !== "") attributes.push(["stroke-dasharray", dash]);
  attributes.push(["stroke-linecap", cap]);
  append("line", attributes);
};

export const appendCircle = cx => cy => radius => fill => () =>
  append("circle", [["cx", cx], ["cy", cy], ["r", radius], ["fill", fill]]);

export const appendPath = d => stroke => width => () =>
  append("path", [["d", d], ["fill", "none"], ["stroke", stroke], ["stroke-width", width]]);
