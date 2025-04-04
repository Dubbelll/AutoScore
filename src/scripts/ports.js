const element = document.getElementById("app");
const app = Elm.App.embed(element);
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
    zoom: 10,
    movementStartX: 0,
    movementStartY: 0,
    movementStartTop: 0,
    movementStartLeft: 0,
    movementStartWidth: 0,
    movementStartHeight: 0,
    thresholdsBlack: {},
    thresholdsWhite: {}
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

function calculateBoundedOffset(currentX, currentY, widthBoundary, heightBoundary, widthCrop, heightCrop, border) {
    const newTop = state.movementStartTop + (currentY - state.movementStartY);
    const newLeft = state.movementStartLeft + (currentX - state.movementStartX);
    const maxTop = heightBoundary - heightCrop - (border * 2);
    const maxLeft = widthBoundary - widthCrop - (border * 2);
    const newBoundedTop = Math.max(0, Math.min(maxTop, newTop));
    const newBoundedLeft = Math.max(0, Math.min(maxLeft, newLeft));

    return { top: newBoundedTop, left: newBoundedLeft };
}

function calculatePreviewOffset(offset, sizePreview, widthBoundary, heightBoundary, widthCrop, heightCrop, border) {
    const size = Math.min(widthBoundary, heightBoundary);
    const previewTop = (- (offset.top + border)) * (sizePreview / heightCrop);
    const previewLeft = (- (offset.left + border)) * (sizePreview / widthCrop);

    return { top: previewTop, left: previewLeft };
}

function drag(currentX, currentY, boundary, crop, border, image, imagePreview, sizePreview) {
    if (state.isDragging) {
        const widthBoundary = pixelsToNumber(boundary.style.width);
        const widthCrop = pixelsToNumber(crop.style.width);
        const heightBoundary = pixelsToNumber(boundary.style.height);
        const heightCrop = pixelsToNumber(crop.style.height);

        const offset = calculateBoundedOffset(
            currentX,
            currentY,
            widthBoundary,
            heightBoundary,
            widthCrop,
            heightCrop,
            border
        );
        crop.style.top = numberToPixels(offset.top);
        crop.style.left = numberToPixels(offset.left);
        image.style.top = numberToPixels(- (offset.top + border));
        image.style.left = numberToPixels(- (offset.left + border));

        if (imagePreview) {
            const offsetPreview = calculatePreviewOffset(
                offset,
                sizePreview,
                widthBoundary,
                heightBoundary,
                widthCrop,
                heightCrop,
                border
            );
            imagePreview.style.top = numberToPixels(offsetPreview.top);
            imagePreview.style.left = numberToPixels(offsetPreview.left);
        }
    }
}

function calculateFrameDimensions(currentX, currentY, widthBoundary, heightBoundary, topCrop, leftCrop, border) {
    const newWidth = state.movementStartWidth + (currentX - state.movementStartX);
    const newHeight = state.movementStartHeight + (currentY - state.movementStartY);
    const maxWidth = widthBoundary - leftCrop - (border * 2);
    const maxHeight = heightBoundary - topCrop - (border * 2);
    const newBoundedWidth = Math.max(0, Math.min(maxWidth, newWidth));
    const newBoundedHeight = Math.max(0, Math.min(maxHeight, newHeight));

    return { width: newBoundedWidth, height: newBoundedHeight };
}

function resizeByFrame(currentX, currentY, boundary, crop, border, image) {
    if (state.isResizing) {
        const widthBoundary = pixelsToNumber(boundary.style.width);
        const heightBoundary = pixelsToNumber(boundary.style.height);
        const topCrop = pixelsToNumber(crop.style.top);
        const leftCrop = pixelsToNumber(crop.style.left);

        const dimensions = calculateFrameDimensions(
            currentX,
            currentY,
            widthBoundary,
            heightBoundary,
            topCrop,
            leftCrop,
            border
        );
        crop.style.width = numberToPixels(dimensions.width);
        crop.style.height = numberToPixels(dimensions.height);
    }
}

function calculateZoomDimensions(value, size, widthBoundary, heightBoundary, topCrop, leftCrop, border) {
    const newSize = size * (value / 100);
    const maxWidth = widthBoundary - leftCrop - (border * 2);
    const maxHeight = heightBoundary - topCrop - (border * 2);
    const newMax = Math.min(maxWidth, maxHeight);
    const newBoundedWidth = Math.max(0, Math.min(newMax, newSize));
    const newBoundedHeight = Math.max(0, Math.min(newMax, newSize));

    return { width: newBoundedWidth, height: newBoundedHeight };
}

function calculatePreviewDimensions(size, sizePreview, widthBoundary, heightBoundary, widthCrop, heightCrop) {
    const newWidth = widthBoundary * (sizePreview / widthCrop);
    const newHeight = heightBoundary * (sizePreview / heightCrop);

    return { width: newWidth, height: newHeight };
}

function resizeByZoom(value, boundary, crop, border, percentage, image, imagePreview, sizePreview) {
    const widthBoundary = pixelsToNumber(boundary.style.width);
    const heightBoundary = pixelsToNumber(boundary.style.height);
    const topCrop = pixelsToNumber(crop.style.top);
    const leftCrop = pixelsToNumber(crop.style.left);
    const size = Math.min(widthBoundary, heightBoundary);
    const ratio = widthBoundary / heightBoundary;

    const dimensions = calculateZoomDimensions(
        value,
        size,
        widthBoundary,
        heightBoundary,
        topCrop,
        leftCrop,
        border
    );
    crop.style.width = numberToPixels(dimensions.width);
    crop.style.height = numberToPixels(dimensions.height);
    percentage.innerHTML = value.toString() + "%";

    if (imagePreview) {
        const topCrop = pixelsToNumber(crop.style.top);
        const leftCrop = pixelsToNumber(crop.style.left);
        const widthCrop = pixelsToNumber(crop.style.width);
        const heightCrop = pixelsToNumber(crop.style.height);
        const offsetCrop = { top: topCrop, left: leftCrop };

        const offsetPreview = calculatePreviewOffset(
            offsetCrop,
            sizePreview,
            widthBoundary,
            heightBoundary,
            dimensions.width,
            dimensions.height,
            border
        );
        const dimensionsPreview = calculatePreviewDimensions(
            size,
            sizePreview,
            widthBoundary,
            heightBoundary,
            widthCrop,
            heightCrop
        );
        imagePreview.style.top = numberToPixels(offsetPreview.top);
        imagePreview.style.left = numberToPixels(offsetPreview.left);
        imagePreview.style.width = numberToPixels(dimensionsPreview.width);
        imagePreview.style.height = numberToPixels(dimensionsPreview.height);
    }
}

function addEventListenersForDragging(boundary, crop, border, area, image, imagePreview, sizePreview) {
    area.addEventListener("mousedown", function (event) {
        initializeDrag(event.pageX, event.pageY, crop);

        event.preventDefault();
    });

    window.addEventListener("mousemove", function (event) {
        drag(event.pageX, event.pageY, boundary, crop, border, image, imagePreview, sizePreview);
    });

    area.addEventListener("touchstart", function (event) {
        initializeDrag(event.touches[0].pageX, event.touches[0].pageY, crop);

        event.preventDefault();
    });

    window.addEventListener("touchmove", function (event) {
        drag(event.touches[0].pageX, event.touches[0].pageY, boundary, crop, border, image, imagePreview, sizePreview);
    });
}

function addEventListenersForResizingByFrame(boundary, crop, border, area, image) {
    area.addEventListener("mousedown", function (event) {
        initializeResize(event.pageX, event.pageY, crop);

        event.preventDefault();
    });

    window.addEventListener("mousemove", function (event) {
        resizeByFrame(event.pageX, event.pageY, boundary, crop, border, image);
    });

    area.addEventListener("touchstart", function (event) {
        initializeResize(event.touches[0].pageX, event.touches[0].pageY, crop);

        event.preventDefault();
    });

    window.addEventListener("touchmove", function (event) {
        resizeByFrame(event.touches[0].pageX, event.touches[0].pageY, boundary, crop, border, image);
    });
}

function addEventListenerForResizingByZoom(boundary, crop, border, increase, decrease, percentage, image, imagePreview, sizePreview) {
    const sensitivity = 60;
    let timer = 0;

    increase.addEventListener("mousedown", function (event) {
        timer = setInterval(function () {
            if (state.zoom - 1 >= 0) {
                state.zoom -= 1;
            }

            resizeByZoom(state.zoom, boundary, crop, border, percentage, image, imagePreview, sizePreview);
        }, sensitivity);
    });

    increase.addEventListener("mouseup", function (event) {
        if (timer !== 0) {
            clearInterval(timer);
        }
    });

    decrease.addEventListener("mousedown", function (event) {
        timer = setInterval(function () {
            if (state.zoom + 1 <= 100) {
                state.zoom += 1;
            }

            resizeByZoom(state.zoom, boundary, crop, border, percentage, image, imagePreview, sizePreview);
        }, sensitivity);
    });

    decrease.addEventListener("mouseup", function (event) {
        if (timer !== 0) {
            clearInterval(timer);
        }
    });

    increase.addEventListener("touchstart", function (event) {
        timer = setInterval(function () {
            if (state.zoom - 1 >= 0) {
                state.zoom -= 1;
            }

            resizeByZoom(state.zoom, boundary, crop, border, percentage, image, imagePreview, sizePreview);
        }, sensitivity);
    });

    increase.addEventListener("touchend", function (event) {
        if (timer !== 0) {
            clearInterval(timer);
        }
    });

    decrease.addEventListener("touchstart", function (event) {
        timer = setInterval(function () {
            if (state.zoom + 1 <= 100) {
                state.zoom += 1;
            }

            resizeByZoom(state.zoom, boundary, crop, border, percentage, image, imagePreview, sizePreview);
        }, sensitivity);
    });

    decrease.addEventListener("touchend", function (event) {
        if (timer !== 0) {
            clearInterval(timer);
        }
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
    const size =
        type === CROP_TYPE_RECTANGLE
            ? Math.min(canvas.width, canvas.height) / 2
            : Math.min(canvas.width, canvas.height) * (state.zoom / 100);
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

    if (type === CROP_TYPE_RECTANGLE) {
        const resize = document.getElementById("crop-" + name + "-resize");
        resize.style.width = resizeSize;
        resize.style.height = resizeSize;
        addEventListenersForDragging(boundary, crop, border, move, image);
        addEventListenersForResizingByFrame(boundary, crop, border, resize, image);
    }
    if (type === CROP_TYPE_CIRCLE) {
        const preview = document.getElementById("crop-" + name + "-preview");
        const imagePreview = document.getElementById("crop-" + name + "-preview-image");
        const zoomIn = document.getElementById("crop-" + name + "-zoom-in");
        const zoomOut = document.getElementById("crop-" + name + "-zoom-out");
        const zoomPercentage = document.getElementById("crop-" + name + "-zoom-percentage");
        const sizePreview = window.innerHeight * 0.15;
        const heightSlider = rem() * 2;

        preview.style.top = numberToPixels(- (sizePreview + (rem() * 5)));
        preview.style.width = numberToPixels(sizePreview);
        preview.style.height = numberToPixels(sizePreview);

        imagePreview.addEventListener("load", function () {
            imagePreview.style.width = numberToPixels(canvas.width * (sizePreview / size));
            imagePreview.style.height = numberToPixels(canvas.height * (sizePreview / size));
            imagePreview.style.top = numberToPixels((- (top + border)) * (sizePreview / size));
            imagePreview.style.left = numberToPixels((- (left + border)) * (sizePreview / size));
        });
        imagePreview.src = canvas.toDataURL("image/png");

        zoomPercentage.innerHTML = state.zoom.toString() + "%";

        addEventListenersForDragging(boundary, crop, border, boundary, image, imagePreview, sizePreview);
        addEventListenerForResizingByZoom(boundary, crop, border, zoomIn, zoomOut, zoomPercentage, image, imagePreview, sizePreview);
    }
}

function calculateFixedAspectDimensions(width, height, maxWidth, maxHeight) {
    let newWidth = width;
    let newHeight = height;
    let newAspectRatio = 0;

    if (maxWidth > maxHeight) {
        if (width > maxWidth && height > maxHeight) {
            newAspectRatio = width / height;
            newWidth = Math.round(maxHeight * newAspectRatio);
            newHeight = maxHeight;
        }
        else if (width > maxWidth) {
            newAspectRatio = width / height;
            newWidth = maxWidth;
            newHeight = Math.round(maxWidth * newAspectRatio);
        }
        else if (height > maxHeight) {
            newAspectRatio = width / height;
            newWidth = Math.round(maxHeight * newAspectRatio);
            newHeight = maxHeight;
        }
    }
    else if (maxHeight > maxWidth) {
        if (height > maxHeight && width > maxWidth) {
            newAspectRatio = height / width;
            newWidth = maxWidth;
            newHeight = Math.round(maxWidth * newAspectRatio);
        }
        else if (height > maxHeight) {
            newAspectRatio = height / width;
            newWidth = Math.round(maxHeight * newAspectRatio);
            newHeight = maxHeight;
        }
        else if (width > maxWidth) {
            newAspectRatio = height / width;
            newWidth = maxWidth;
            newHeight = Math.round(maxWidth * newAspectRatio);
        }
    }
    else {
        if (width > maxWidth && height > maxHeight) {
            newAspectRatio = 1;
            newWidth = Math.min(maxWidth, maxHeight);
            newHeight = Math.min(maxWidth, maxHeight);
        }
    }

    return { width: newWidth, height: newHeight, aspectRatio: newAspectRatio };
}

function calculateAverageColors(pixels) {
    let reds = 0;
    let greens = 0;
    let blues = 0;
    let count = 0;

    for (let i = 0; i < pixels.length; i += 4) {
        if (pixels[i + 3] !== 0) {
            reds += pixels[i];
            greens += pixels[i + 1];
            blues += pixels[i + 2];

            count++;
        }
    }

    const averages = {
        r: reds / count,
        g: greens / count,
        b: blues / count
    };

    return averages;
}

function determineAverageColorsInCrop(name, callback) {
    const canvasColor = document.getElementById("canvas-" + name);
    const canvasTemporary = document.getElementById("canvas-temporary");
    const contextColor = canvasColor.getContext("2d");
    const contextTemporary = canvasTemporary.getContext("2d");
    const crop = document.getElementById("crop-" + name);
    const border = rem() * 0.3;
    const top = pixelsToNumber(crop.style.top) + border;
    const left = pixelsToNumber(crop.style.left) + border;
    const size = Math.min(pixelsToNumber(crop.style.width), pixelsToNumber(crop.style.height));
    const radius = size / 2;
    const image = new Image();
    const cropped = contextColor.getImageData(
        left,
        top,
        size,
        size
    );

    canvasTemporary.width = size;
    canvasTemporary.height = size;
    contextTemporary.putImageData(cropped, 0, 0);

    image.addEventListener("load", function () {
        contextTemporary.clearRect(0, 0, canvasTemporary.width, canvasTemporary.height);
        contextTemporary.beginPath();
        contextTemporary.arc(radius, radius, radius, 0, Math.PI * 2, true);
        contextTemporary.closePath();
        contextTemporary.clip();
        contextTemporary.drawImage(image, 0, 0);

        const pixels = contextTemporary.getImageData(0, 0, canvasTemporary.width, canvasTemporary.height).data;
        const averages = calculateAverageColors(pixels);

        callback(averages);
    });
    image.src = canvasTemporary.toDataURL("image/png");
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
            const dimensions = calculateFixedAspectDimensions(
                image.width,
                image.height,
                window.innerWidth,
                window.innerHeight
            );

            canvas.width = dimensions.width;
            canvas.height = dimensions.height;
            context.drawImage(image, 0, 0, dimensions.width, dimensions.height);

            const imageData =
                {
                    image: image,
                    width: dimensions.width,
                    height: dimensions.height
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
    const options = { video: { width: 1920, height: 1080, facingMode: "environment" }, audio: false };

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
    const canvasTemporary = document.getElementById("canvas-temporary");
    const contextInput = canvasInput.getContext("2d");
    const contextBlack = canvasBlack.getContext("2d");
    const contextWhite = canvasWhite.getContext("2d");
    const contextOutput = canvasOutput.getContext("2d");
    const contextTemporary = canvasTemporary.getContext("2d");
    const crop = document.getElementById("crop-input");
    const top = pixelsToNumber(crop.style.top);
    const left = pixelsToNumber(crop.style.left);
    const width = pixelsToNumber(crop.style.width);
    const height = pixelsToNumber(crop.style.height);
    const image = new Image();
    const cropped = contextInput.getImageData(
        left,
        top,
        width,
        height
    );
    const dimensions = calculateFixedAspectDimensions(
        width,
        height,
        window.innerWidth * 0.5,
        window.innerHeight * 0.5
    );

    canvasTemporary.width = width;
    canvasTemporary.height = height;
    contextTemporary.putImageData(cropped, 0, 0);

    image.addEventListener("load", function () {
        canvasBlack.width = dimensions.width;
        canvasWhite.width = dimensions.width;
        canvasOutput.width = dimensions.width;
        canvasBlack.height = dimensions.height;
        canvasWhite.height = dimensions.height;
        canvasOutput.height = dimensions.height;
        contextBlack.drawImage(image, 0, 0, dimensions.width, dimensions.height);
        contextWhite.drawImage(image, 0, 0, dimensions.width, dimensions.height);
        contextOutput.drawImage(image, 0, 0, dimensions.width, dimensions.height);

        app.ports.croppingSuccessful.send(true);
    });
    image.src = canvasTemporary.toDataURL("image/png");
});

app.ports.startPickingBlack.subscribe(function (bool) {
    initializeCropFrame("color-black", CROP_TYPE_CIRCLE);
});

app.ports.pickBlack.subscribe(function (bool) {
    function useAverages(averages) {
        state.thresholdsBlack = averages;

        app.ports.pickingBlackSuccessful.send(true);
    }

    determineAverageColorsInCrop("color-black", useAverages);
});

app.ports.startPickingWhite.subscribe(function (bool) {
    initializeCropFrame("color-white", CROP_TYPE_CIRCLE);
});

app.ports.pickWhite.subscribe(function (bool) {
    function useAverages(averages) {
        state.thresholdsWhite = averages;

        app.ports.pickingWhiteSuccessful.send(true);
    }

    const averages = determineAverageColorsInCrop("color-white", useAverages);
});

app.ports.startProcessing.subscribe(function (bool) {
    const canvas = document.getElementById("canvas-output");
    const context = canvas.getContext("2d");
    const pixels = context.getImageData(0, 0, canvas.width, canvas.height).data;

    function isBlack(r, g, b) {
        return r <= state.thresholdsBlack.r
            && g <= state.thresholdsBlack.g
            && b <= state.thresholdsBlack.b;
    }

    function isWhite(r, g, b) {
        return r >= state.thresholdsWhite.r
            && g >= state.thresholdsWhite.g
            && b >= state.thresholdsWhite.b;
    }

    const matches = [];
    for (let y = 0; y < canvas.height; y++) {
        for (let x = 0; x < canvas.width; x++) {
            const i = (y * canvas.width + x) * 4;
            const black = isBlack(pixels[i], pixels[i + 1], pixels[i + 2]);
            const white = isWhite(pixels[i], pixels[i + 1], pixels[i + 2]);

            if (black) {
                context.fillStyle = "#F44336";
                context.fillRect(x, y, 3, 3);
                matches.push({ x: x, y: y, color: 0 })
            }
            if (white) {
                context.fillStyle = "#03A9F4";
                context.fillRect(x, y, 3, 3);
                matches.push({ x: x, y: y, color: 1 });
            }
        }
    }

    const probabilities = {};
    for (let y = 1; y < 20; y++) {
        for (let x = 1; x < 20; x++) {
            const key = y.toString() + "-" + x.toString();
            probabilities[key] = { stone: 0, black: 0, white: 0 };
        }
    }

    for (let i = 0; i < matches.length; i++) {
        const match = matches[i];
        const percentageX = match.x / canvas.width;
        const percentageY = match.y / canvas.height;
        const x = Math.min(19, Math.ceil(19 * percentageX));
        const y = Math.min(19, Math.ceil(19 * percentageY));

        if (x > 0 && y > 0) {
            const key = y.toString() + "-" + x.toString();
            const probability = probabilities[key];

            probability.stone += 1;
            if (match.color === 0) {
                probability.black += 1;
            }
            if (match.color === 1) {
                probability.white += 1;
            }
        }
    }

    app.ports.processingSuccessful.send(probabilities);
});