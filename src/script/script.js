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
    cropInput: {},
    cropBlack: {},
    cropWhite: {}
};

function setViewportMinimum() {
    document.documentElement.style.fontSize = (Math.min(window.innerWidth, window.innerHeight) * 0.01) + "px";
};

window.addEventListener("load", function () {
    setViewportMinimum();
});

window.addEventListener("resize", function () {
    setViewportMinimum();
});

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
    const canvas = document.getElementById("canvas-input");
    const width = state.input.width / 2;
    const height = state.input.height / 2;
    const crop = new ImageCropper(canvas, width - (width / 2), height - (height / 2), width, height, false);

    crop.setImage(state.input.image);

    state.cropInput = crop;
});

app.ports.cropPhoto.subscribe(function (bool) {
    const canvasBlack = document.getElementById("canvas-color-black");
    const canvasWhite = document.getElementById("canvas-color-white");
    const canvasOutput = document.getElementById("canvas-output");
    const contextBlack = canvasBlack.getContext("2d");
    const contextWhite = canvasWhite.getContext("2d");
    const contextOutput = canvasOutput.getContext("2d");
    const image = new Image();

    image.addEventListener("load", function () {
        canvasBlack.width = image.width;
        canvasWhite.width = image.width;
        canvasBlack.height = image.height;
        canvasWhite.height = image.height;
        canvasOutput.width = image.width;
        canvasOutput.height = image.height;
        contextBlack.drawImage(image, 0, 0);
        contextWhite.drawImage(image, 0, 0);
        contextOutput.drawImage(image, 0, 0);

        const crop = {
            x: Math.round((window.innerWidth - image.width) / 2),
            y: Math.round((window.innerHeight - image.height) / 2),
            width: image.width,
            height: image.height
        }

        const cropData = {
            image: image,
            width: image.width,
            height: image.height
        };

        state.cropped = cropData;
        app.ports.croppingSuccessful.send(crop);
    });
    image.src = state.cropInput.getCroppedImage().src;
});

app.ports.startPickingBlack.subscribe(function (bool) {
    const canvas = document.getElementById("canvas-color-black");

    const width = state.cropped.width / 8;
    const height = state.cropped.height / 8;
    const crop = new ImageCropper(canvas, width - (width / 2), height - (height / 2), width, height, false);

    crop.setImage(state.cropped.image);

    state.cropBlack = crop;
});

app.ports.pickBlack.subscribe(function (bool) {
    const canvas = document.getElementById("canvas-color-black");
    const context = canvas.getContext("2d");
    const image = new Image();

    image.addEventListener("load", function () {
        const cropData = {
            image: image,
            width: image.width,
            height: image.height
        };

        state.black = cropData;
        app.ports.pickingBlackSuccessful.send(true);
    });
    image.src = state.cropBlack.getCroppedImage().src;
});

app.ports.startPickingWhite.subscribe(function (bool) {
    const canvas = document.getElementById("canvas-color-white");

    const width = state.cropped.width / 8;
    const height = state.cropped.height / 8;
    const crop = new ImageCropper(canvas, width - (width / 2), height - (height / 2), width, height, false);

    crop.setImage(state.cropped.image);

    state.cropWhite = crop;
});

app.ports.pickWhite.subscribe(function (bool) {
    const canvas = document.getElementById("canvas-color-white");
    const context = canvas.getContext("2d");
    const image = new Image();

    image.addEventListener("load", function () {
        const cropData = {
            image: image,
            width: image.width,
            height: image.height
        };

        state.white = cropData;
        app.ports.pickingWhiteSuccessful.send(true);
    });
    image.src = state.cropWhite.getCroppedImage().src;
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