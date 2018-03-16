const element = document.getElementById("app");
const flags =
    {
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
    processed: [],
    crop: {}
};

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

    canvas.width = window.innerWidth;
    canvas.height = window.innerHeight;
    context.drawImage(video, 0, 0, window.innerWidth, window.innerHeight);

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

    state.crop = crop;
});

app.ports.cropPhoto.subscribe(function (bool) {
    const canvas = document.getElementById("canvas-output");
    const context = canvas.getContext("2d");
    const cropped = state.crop.getCroppedImage();
    const image = new Image();

    image.addEventListener("load", function () {
        canvas.width = image.width;
        canvas.height = image.height;
        context.drawImage(image, 0, 0);

        const crop = {
            x: Math.round((window.innerWidth - image.width) / 2),
            y: Math.round((window.innerHeight - image.height) / 2),
            width: image.width,
            height: image.height
        }
        app.ports.croppingSuccessful.send(crop);
    });
    image.src = cropped.src;
});

app.ports.startProcessing.subscribe(function (bool) {
    const thresholdBlack = 32.5;
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
                console.log(detection);
                app.ports.stoneDetected.send(detection);
            });
        }
    });

    tracking.track("#canvas-output", tracker);
});