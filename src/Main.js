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
