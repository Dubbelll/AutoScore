const element = document.getElementById("app");
const flags = {
    version: CONFIG_API_VERSION,
    baseURL: CONFIG_API_BASE_URL
};
const app = Elm.App.embed(element, flags);
const state = {
    input: {
        image: new Image(),
        width: 0,
        height: 0
    },
    cropped: {
        image: new Image(),
        width: 0,
        height: 0
    },
    black: {
        image: new Image(),
        width: 0,
        height: 0
    },
    white: {
        image: new Image(),
        width: 0,
        height: 0
    },
    isDragging: false,
    isResizing: false,
    movementStartX: 0,
    movementStartY: 0,
    movementStartTop: 0,
    movementStartLeft: 0,
    movementStartWidth: 0,
    movementStartHeight: 0,
};

const CROP_TYPE_RECTANGLE = 1;
const CROP_TYPE_CIRCLE = 2;

window.addEventListener("load", function () {
    setViewportMinimum();
});

window.addEventListener("resize", function () {
    setViewportMinimum();
});

window.addEventListener("mouseup", function () {
    state.isDragging = false;
    state.isResizing = false;
});

window.addEventListener("touchend", function () {
    state.isDragging = false;
    state.isResizing = false;
});

function setViewportMinimum() {
    document.documentElement.style.fontSize = (Math.min(window.innerWidth, window.innerHeight) * 0.01) + "px";
};

function rem() {
    return pixelsToNumber(document.documentElement.style.fontSize);
}

function numberToPixels(number) {
    return "" + number + "px";
}

function pixelsToNumber(pixels) {
    return parseFloat(pixels.split("px")[0]);
}

function initializeDrag(startX, startY, crop) {
    state.isDragging = true;
    state.movementStartX = startX;
    state.movementStartY = startY;
    state.movementStartTop = pixelsToNumber(crop.style.top);
    state.movementStartLeft = pixelsToNumber(crop.style.left);
}

function initializeResize(startX, startY, crop) {
    state.isResizing = true;
    state.movementStartX = startX;
    state.movementStartY = startY;
    state.movementStartWidth = pixelsToNumber(crop.style.width);
    state.movementStartHeight = pixelsToNumber(crop.style.height);
}

function drag(currentX, currentY, boundary, crop, image, border) {
    if (state.isDragging) {
        const newTop = state.movementStartTop + (currentY - state.movementStartY);
        const newLeft = state.movementStartLeft + (currentX - state.movementStartX);
        const maxTop = pixelsToNumber(boundary.style.height) - pixelsToNumber(crop.style.height) - (border * 2);
        const maxLeft = pixelsToNumber(boundary.style.width) - pixelsToNumber(crop.style.width) - (border * 2);
        const newBoundedTop = Math.max(0, Math.min(maxTop, newTop));
        const newBoundedLeft = Math.max(0, Math.min(maxLeft, newLeft));
        crop.style.top = numberToPixels(newBoundedTop);
        crop.style.left = numberToPixels(newBoundedLeft);
        image.style.top = numberToPixels(- (newBoundedTop + border));
        image.style.left = numberToPixels(- (newBoundedLeft + border));
    }
}

function resizeByFrame(currentX, currentY, boundary, crop, image, border) {
    if (state.isResizing) {
        const newWidth = state.movementStartWidth + (currentX - state.movementStartX);
        const newHeight = state.movementStartHeight + (currentY - state.movementStartY);
        const maxWidth = pixelsToNumber(boundary.style.width) - pixelsToNumber(crop.style.left) - (border * 2);
        const maxHeight = pixelsToNumber(boundary.style.height) - pixelsToNumber(crop.style.top) - (border * 2);
        const newBoundedWidth = Math.max(0, Math.min(maxWidth, newWidth));
        const newBoundedHeight = Math.max(0, Math.min(maxHeight, newHeight));
        crop.style.width = numberToPixels(newBoundedWidth);
        crop.style.height = numberToPixels(newBoundedHeight);
        image.style.width = numberToPixels(- (newBoundedWidth + border));
        image.style.height = numberToPixels(- (newBoundedHeight + border));
    }
}

function resizeBySlider(value, boundary, crop, image, border) {
    const boundaryWidth = pixelsToNumber(boundary.style.width);
    const boundaryHeight = pixelsToNumber(boundary.style.width);
    const size = Math.min(boundaryWidth, boundaryHeight);
    const newWidth = size * (value / 100);
    const newHeight = size * (value / 100);
    const maxWidth = boundaryWidth - pixelsToNumber(crop.style.left) - (border * 2);
    const maxHeight = boundaryHeight - pixelsToNumber(crop.style.top) - (border * 2);
    const newBoundedWidth = Math.max(0, Math.min(maxWidth, newWidth));
    const newBoundedHeight = Math.max(0, Math.min(maxHeight, newHeight));
    crop.style.width = numberToPixels(newBoundedWidth);
    crop.style.height = numberToPixels(newBoundedHeight);
    image.style.width = numberToPixels(- (newBoundedWidth + border));
    image.style.height = numberToPixels(- (newBoundedHeight + border));
}

function addEventListenersForDragging(boundary, crop, image, area, border) {
    area.addEventListener("mousedown", function (event) {
        initializeDrag(event.pageX, event.pageY, crop);

        event.preventDefault();
    });

    window.addEventListener("mousemove", function (event) {
        drag(event.pageX, event.pageY, boundary, crop, image, border);
    });

    area.addEventListener("touchstart", function (event) {
        initializeDrag(event.touches[0].pageX, event.touches[0].pageY, crop);

        event.preventDefault();
    });

    window.addEventListener("touchmove", function (event) {
        drag(event.touches[0].pageX, event.touches[0].pageY, boundary, crop, image, border);
    });
}

function addEventListenersForResizingByFrame(boundary, crop, image, area, border) {
    area.addEventListener("mousedown", function (event) {
        initializeResize(event.pageX, event.pageY, crop);

        event.preventDefault();
    });

    window.addEventListener("mousemove", function (event) {
        resizeByFrame(event.pageX, event.pageY, boundary, crop, image, border);
    });

    area.addEventListener("touchstart", function (event) {
        initializeResize(event.touches[0].pageX, event.touches[0].pageY, crop);

        event.preventDefault();
    });

    window.addEventListener("touchmove", function (event) {
        resizeByFrame(event.touches[0].pageX, event.touches[0].pageY, boundary, crop, image, border);
    });
}

function addEventListenerForResizingBySlider(boundary, crop, image, slider, border) {
    slider.addEventListener("input", function (event) {
        resizeBySlider(parseFloat(event.target.value), boundary, crop, image, border);
    });
}

function initializeCropFrame(name, type) {
    const canvas = document.getElementById("canvas-" + name);
    const fader = document.getElementById("crop-" + name + "-fader");
    const boundary = document.getElementById("crop-" + name + "-boundary");
    const crop = document.getElementById("crop-" + name);
    const image = document.getElementById("crop-" + name + "-image");
    const move = document.getElementById("crop-" + name + "-move");
    const border = rem() * 0.3;
    const size = Math.min(canvas.width, canvas.height) / 2;
    const top = (canvas.height / 2) - (size / 2);
    const left = (canvas.width / 2) - (size / 2);
    const widthInput = numberToPixels(canvas.width);
    const widthCrop = numberToPixels(size);
    const heightInput = numberToPixels(canvas.height);
    const heightCrop = numberToPixels(size);
    const topCrop = numberToPixels(top);
    const topImage = numberToPixels(- (top + border));
    const leftCrop = numberToPixels(left);
    const leftImage = numberToPixels(- (left + border));
    const resizeSize = numberToPixels(size * 0.15);

    fader.style.width = widthInput;
    fader.style.height = heightInput;

    boundary.style.width = widthInput;
    boundary.style.height = heightInput;

    crop.style.width = widthCrop;
    crop.style.height = heightCrop;
    crop.style.top = topCrop;
    crop.style.left = leftCrop;

    image.addEventListener("load", function () {
        image.style.width = widthInput;
        image.style.height = heightInput;
        image.style.top = topImage;
        image.style.left = leftImage;
    });
    image.src = canvas.toDataURL("image/png");

    addEventListenersForDragging(boundary, crop, image, move, border);
    if (type === CROP_TYPE_RECTANGLE) {
        const resize = document.getElementById("crop-" + name + "-resize");
        resize.style.width = resizeSize;
        resize.style.height = resizeSize;
        addEventListenersForResizingByFrame(boundary, crop, image, resize, border);
    }
    if (type === CROP_TYPE_CIRCLE) {
        const slider = document.getElementById("crop-" + name + "-slider");
        const widthBoundary = pixelsToNumber(boundary.style.width);
        const heightBoundary = pixelsToNumber(boundary.style.height);
        const heightSlider = rem() * 2;
        slider.style.top = numberToPixels((heightBoundary / 2) - (heightSlider / 2));
        slider.style.left = numberToPixels((widthBoundary / 2) + rem() * 10);
        slider.style.width = boundary.style.height;
        addEventListenerForResizingBySlider(boundary, crop, image, slider, border);
    }
}

app.ports.useFile.subscribe(function (id) {
    const element = document.getElementById(id);
    if (element === null || element.files.length === 0) {
        return;
    }

    const canvas = document.getElementById("canvas-input");
    const context = canvas.getContext("2d");
    const file = element.files[0];
    const reader = new FileReader();
    const image = new Image();

    reader.addEventListener("load", function () {
        image.addEventListener("load", function () {
            let width = image.width;
            let height = image.height;
            let aspectRatio = 1;

            if (image.width > image.height) {
                aspectRatio = image.width / image.height;

                if (image.width > window.innerWidth) {
                    width = window.innerWidth;
                    height = Math.round(width / aspectRatio);
                }
            }
            else if (image.height > image.width) {
                aspectRatio = image.height / image.width;

                if (image.height > window.innerHeight) {
                    height = window.innerHeight;
                    width = Math.round(height / aspectRatio);
                }
            }
            else {
                aspectRatio = 1;

                if (image.width > window.innerWidth && image.height > window.innerHeight) {
                    width = Math.min(window.innerWidth, window.innerHeight);
                    height = width;
                }
            }

            canvas.width = width;
            canvas.height = height;
            context.drawImage(image, 0, 0, width, height);

            const imageData =
                {
                    image: image,
                    width: width,
                    height: height
                };

            state.input = imageData;
            app.ports.inputSuccessful.send(true);
        });
        image.src = reader.result;
    });

    reader.readAsDataURL(file);
});

app.ports.startCamera.subscribe(function (bool) {
    const canvas = document.getElementById("canvas-input");
    const video = document.getElementById("video");
    const options = { video: { facingMode: "environment" }, audio: false };

    navigator.mediaDevices.getUserMedia(options)
        .then(function (stream) {
            video.srcObject = stream;
            video.play();
        })
        .catch(function (err) {
            return;
        });

    video.addEventListener("canplay", function () {
        let width = 0;
        let height = 0;
        if (window.innerWidth > window.innerHeight) {
            height = window.innerHeight;
            width = video.videoWidth / (video.videoHeight / height);
        }
        else {
            width = window.innerWidth;
            height = video.videoHeight / (video.videoWidth / width);
        }
        canvas.width = width;
        canvas.height = height;
        video.width = width;
        video.height = height;

        app.ports.cameraStarted.send(true);
    });
});

app.ports.stopCamera.subscribe(function (bool) {
    const video = document.getElementById("video");

    video.pause();

    app.ports.cameraStopped.send(false);
});

app.ports.takePhoto.subscribe(function (bool) {
    const canvas = document.getElementById("canvas-input");
    const context = canvas.getContext("2d");
    const video = document.getElementById("video");
    const image = new Image();

    context.drawImage(video, 0, 0, canvas.width, canvas.height);

    image.addEventListener("load", function () {
        const imageData =
            {
                image: image,
                width: image.width,
                height: image.height
            };

        state.input = imageData;
        app.ports.inputSuccessful.send(true);
    });
    image.src = canvas.toDataURL("image/png");
});

app.ports.startCropping.subscribe(function (bool) {
    initializeCropFrame("input", CROP_TYPE_RECTANGLE);
});

app.ports.cropPhoto.subscribe(function (bool) {
    const canvasInput = document.getElementById("canvas-input");
    const canvasBlack = document.getElementById("canvas-color-black");
    const canvasWhite = document.getElementById("canvas-color-white");
    const canvasOutput = document.getElementById("canvas-output");
    const contextInput = canvasInput.getContext("2d");
    const contextBlack = canvasBlack.getContext("2d");
    const contextWhite = canvasWhite.getContext("2d");
    const contextOutput = canvasOutput.getContext("2d");
    const crop = document.getElementById("crop-input");
    const top = pixelsToNumber(crop.style.top);
    const left = pixelsToNumber(crop.style.left);
    const width = pixelsToNumber(crop.style.width);
    const height = pixelsToNumber(crop.style.height);
    const cropped = contextInput.getImageData(
        left,
        top,
        width,
        height
    );

    canvasBlack.width = width;
    canvasWhite.width = width;
    canvasOutput.width = width;
    canvasBlack.height = height;
    canvasWhite.height = height;
    canvasOutput.height = height;
    contextBlack.putImageData(cropped, 0, 0);
    contextWhite.putImageData(cropped, 0, 0);
    contextOutput.putImageData(cropped, 0, 0);

    app.ports.croppingSuccessful.send(true);
});

app.ports.startPickingBlack.subscribe(function (bool) {
    initializeCropFrame("color-black", CROP_TYPE_CIRCLE);
});

app.ports.pickBlack.subscribe(function (bool) {
    const canvas = document.getElementById("canvas-color-black");
    const context = canvas.getContext("2d");
    const crop = document.getElementById("crop-color-black");
    const top = pixelsToNumber(crop.style.top);
    const left = pixelsToNumber(crop.style.left);
    const width = pixelsToNumber(crop.style.width);
    const height = pixelsToNumber(crop.style.height);
    const cropped = context.getImageData(
        left,
        top,
        width,
        height
    );

    app.ports.pickingBlackSuccessful.send(true);
});

app.ports.startPickingWhite.subscribe(function (bool) {
    initializeCropFrame("color-white", CROP_TYPE_CIRCLE);
});

app.ports.pickWhite.subscribe(function (bool) {
    const canvas = document.getElementById("canvas-color-white");
    const context = canvas.getContext("2d");
    const crop = document.getElementById("crop-color-white");
    const top = pixelsToNumber(crop.style.top);
    const left = pixelsToNumber(crop.style.left);
    const width = pixelsToNumber(crop.style.width);
    const height = pixelsToNumber(crop.style.height);
    const cropped = context.getImageData(
        left,
        top,
        width,
        height
    );

    app.ports.pickingWhiteSuccessful.send(true);
});

app.ports.startProcessing.subscribe(function (bool) {
    const canvas = document.getElementById("canvas-output");
    const context = canvas.getContext("2d");
    const pixels = context.getImageData(0, 0, canvas.width, canvas.height).data;

    console.log("processing size: " + canvas.width + " x " + canvas.height);

    const thresholdBlack = 32.5;
    const thresholdWhite = 150;

    function isMatch(matches, size, x, y) {
        return matches.filter(function (match) {
            return (x >= match.x && x <= match.x + size)
                && (y >= match.y && y <= match.y + size);
        }).length > 0;
    }

    function averageColors(pixels) {
        let reds = 0;
        let greens = 0;
        let blues = 0;

        for (let i = 0; i < pixels.length; i += 4) {
            reds += pixels[i];
            greens += pixels[i + 1];
            blues += pixels[i + 2];
        }

        const averages = {
            r: reds / pixels.length,
            g: greens / pixels.length,
            b: blues / pixels.length
        };

        return averages;
    }

    function isBlack(r, g, b) {
        return r <= thresholdBlack
            && g <= thresholdBlack
            && b <= thresholdBlack;
    }

    function isWhite(r, g, b) {
        return r >= thresholdWhite
            && g >= thresholdWhite
            && b >= thresholdWhite;
    }

    function isMostlyBlack(averages) {
        return averages.r <= thresholdBlack
            && averages.g <= thresholdBlack
            && averages.b <= thresholdBlack;
    }

    function isMostlyWhite(averages) {
        return averages.r >= thresholdWhite
            && averages.g >= thresholdWhite
            && averages.b >= thresholdWhite;
    }

    const size = 25;
    const matches = [];
    for (let y = 0; y < canvas.height; y++) {
        for (let x = 0; x < canvas.width; x++) {
            const i = (y * canvas.width + x) * 4;
            const black = isBlack(pixels[i], pixels[i + 1], pixels[i + 2]);
            const white = isWhite(pixels[i], pixels[i + 1], pixels[i + 2]);

            if (black) {
                context.fillStyle = "#F44336";
                context.fillRect(x, y, 3, 3);
            }
            if (white) {
                context.fillStyle = "#03A9F4";
                context.fillRect(x, y, 3, 3);
            }

            /* if (!hasMatched) {
                const vector = context.getImageData(x, y, size, size).data;
                const averages = averageColors(vector);
                const isBlack = isMostlyBlack(averages);
                const isWhite = isMostlyWhite(averages);

                if (isBlack) {
                    context.fillStyle = "#F44336";
                    context.fillRect(x, y, 3, 3);
                    matches.push({ x: x, y: y, color: "black" });
                }
                if (isWhite) {
                    context.fillStyle = "#03A9F4";
                    context.fillRect(x, y, 3, 3);
                    matches.push({ x: x, y: y, color: "white" });
                }
            } */
        }
    }
    console.log(matches);


    /* const thresholdBlack = 32.5;
    const thresholdWhite = 150;

    tracking.ColorTracker.registerColor("black", function (r, g, b) {
        return r <= thresholdBlack && g <= thresholdBlack && b <= thresholdBlack;
    });

    tracking.ColorTracker.registerColor("white", function (r, g, b) {
        return r >= thresholdWhite && g >= thresholdWhite && b >= thresholdWhite;
    });

    const tracker = new tracking.ColorTracker(["black", "white"]);
    tracker.setMinDimension(15);

    tracker.on("track", function (event) {
        if (event.data.length === 0) {
            return;
        } else {
            event.data.map(function (detection) {
                app.ports.stoneDetected.send(detection);
            });

            app.ports.processingSuccessful.send(true);
        }
    });

    tracking.track("#canvas-output", tracker); */
});