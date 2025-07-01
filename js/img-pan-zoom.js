const images = document.querySelectorAll("img[data-pan-zoom]");
images.forEach((image) => {
  addPanZoom(image);
});

var inOverlayButton = false;

function createOverlayButton(value) {
  const buttonStyle = `
    background: rgba(0, 0, 0, 0.7);
    color: white;
    border: none;
    border-radius: 4px;
    width: 32px;
    height: 32px;
    font-weight: bold;
    cursor: pointer;
    display: flex;
    align-items: center;
    justify-content: center;
  `;
  
  const button = document.createElement("button");
  button.innerHTML = value;
  button.style.cssText = buttonStyle;
  button.addEventListener("mouseenter", () => {
    button.style.backgroundColor = "rgba(0, 0, 0, 0.9)";
    inOverlayButton = true;
  });
  button.addEventListener("mouseleave", () => {
    button.style.backgroundColor = "rgba(0, 0, 0, 0.7)";
    inOverlayButton = false;
  });
  return button;
}

function update(state, image) {
  requestAnimationFrame(() => {
    image.style.transform = `translate(${state.translateX}px, ${state.translateY}px)`;
    image.style.width = `${state.width}px`;
  });
}

function addPanZoom(image) {
  const viewport = document.createElement("div");
  viewport.style.cssText = `
    position: relative;
    overflow: hidden;
    cursor: grab;
    user-select: text;
    touch-action: none;
  `;
  
  const buttonContainer = document.createElement("div");
  buttonContainer.style.cssText = `
    position: absolute;
    top: 10px;
    right: 10px;
    display: flex;
    flex-direction: column;
    gap: 5px;
  `;
  
  const zoomInButton = createOverlayButton("+");
  zoomInButton.title = "Zoom In";
  buttonContainer.appendChild(zoomInButton);
  
  const zoomOutButton = createOverlayButton("−");
  zoomOutButton.title = "Zoom Out";
  buttonContainer.appendChild(zoomOutButton);

  const resetButton = createOverlayButton("↺");
  resetButton.title = "Reset Zoom";
  buttonContainer.appendChild(resetButton);

  const helpButtonContainer = document.createElement("div");
  helpButtonContainer.style.cssText = `
    position: absolute;
    bottom: 10px;
    right: 10px;
  `;

  const helpButton = createOverlayButton("?");
  helpButton.title = "Help";
  helpButtonContainer.appendChild(helpButton);

  const helpDialog = document.createElement("div");
  helpDialog.style.cssText = `
    background: rgba(0, 0, 0, 0.9);
    color: white;
    display: none;
    position: absolute;
    top: 50%;
    left: 50%;
    width: max-content;
    transform: translate(-50%, -50%);
    padding: 20px;
    border-radius: 8px;
  `;

  helpDialog.innerHTML = `
    <button id="helpCloseButton" style="position: absolute; top: 8px; right: 8px; background: none; color: white; border: none; font-size: 18px; cursor: pointer; width: 24px; height: 24px; display: flex; align-items: center; justify-content: center; border-radius: 2px;">⨯</button>
    <h3 style="margin: 0 0 0.6rem 0; font-size: 1.2em;">Controls</h3>
    <ul style="margin: 0 0 0 0; padding-left: 1em;">
      <li>Left-click/touch and drag to pan</li>
      <li><strong>+</strong>/scroll up to zoom in</li>
      <li><strong>−</strong>/scroll down to zoom out</li>
      <li><strong>↺</strong> to reset zoom/pan</li>
    </ul>
  `;

  const closeHelpButton = helpDialog.querySelector('#helpCloseButton');
  closeHelpButton.addEventListener("click", (event) => {
    helpDialog.style.display = "none";
  });

  viewport.addEventListener("click", (event) => {
    if (helpDialog.style.display !== "none" && !helpDialog.contains(event.target)) {
        helpDialog.style.display = "none";
    }
  });

  image.parentNode.insertBefore(viewport, image);
  viewport.appendChild(image);
  viewport.appendChild(buttonContainer);
  viewport.appendChild(helpButtonContainer);
  viewport.appendChild(helpDialog);

  if (image.width !== undefined) {
    viewport.style.width = `${image.width}px`;
    image.removeAttribute("width");
  }

  if (image.height !== undefined) {
    viewport.style.height = `${image.height}px`;
    image.removeAttribute("height");
  }
  
  image.style.transformOrigin = "top left";

  const originalState = (() => {
    const viewportRect = viewport.getBoundingClientRect();    
    image.style.width = `${viewportRect.width}px`;
    
    const imageRect = image.getBoundingClientRect();
    return {
      width: viewportRect.width,
      translateX: 0,
      translateY: (viewportRect.height - imageRect.height) / 2
    };
  })();

  function reset() {
    state.width = originalState.width;
    state.translateX = originalState.translateX;
    state.translateY = originalState.translateY;

    update(state, image);
  }
  
  var state = {}; reset();
  requestAnimationFrame(() => {
    requestAnimationFrame(() => {
      image.style.transition = "all 0.1s ease-out";
    });
  });

  function zoom(factor, focus) {
    state.width = factor * state.width;

    const newTranslateX = focus.x - (focus.x- state.translateX) * factor;
    const newTranslateY = focus.y - (focus.y - state.translateY) * factor;
    state.translateX = newTranslateX;
    state.translateY = newTranslateY;

    update(state, image);
  }
  
  viewport.addEventListener("wheel", (event) => {
    event.preventDefault();
    
    const viewportRect = viewport.getBoundingClientRect();
    const mouseX = event.clientX - viewportRect.left;
    const mouseY = event.clientY - viewportRect.top;
    zoom(event.deltaY > 0 ? 0.8 : 1.2, {x: mouseX, y: mouseY});
  });

  var dragging = false;
  var lastPointerX = undefined;
  var lastPointerY = undefined;
  viewport.addEventListener("pointerdown", (event) => {
    if (!inOverlayButton && event.button === 0 /* left click */) {
      event.preventDefault();
      
      dragging = true;
      lastPointerX = event.clientX;
      lastPointerY = event.clientY;
      viewport.style.cursor = "grabbing";
    }
  });

  document.addEventListener("pointermove", (event) => {
    if (dragging) {
      const deltaX = event.clientX - lastPointerX;
      const deltaY = event.clientY - lastPointerY;

      state.translateX += deltaX;
      state.translateY += deltaY;
      update(state, image);

      lastPointerX = event.clientX;
      lastPointerY = event.clientY;
    }
  });

  document.addEventListener("pointerup", (event) => {
    if (dragging && event.button === 0 /* left click */) {
      dragging = false;
      viewport.style.cursor = "grab";
    }
  });

  zoomInButton.addEventListener("click", (event) => {
    const viewportRect = viewport.getBoundingClientRect();
    zoom(1.2, {x: viewportRect.width / 2, y: viewportRect.height / 2});
  });
  
  zoomOutButton.addEventListener("click", (event) => {
    const viewportRect = viewport.getBoundingClientRect();
    zoom(0.8, {x: viewportRect.width / 2, y: viewportRect.height / 2});
  });

  resetButton.addEventListener("click", (event) => {
    reset();
  });

  helpButton.addEventListener("click", (event) => {
    event.stopPropagation();
    if (helpDialog.style.display === "none") {
      helpDialog.style.display = "block";
    } else {
      helpDialog.style.display = "none";
    }
  });
}
