document.addEventListener("DOMContentLoaded", function () {
    const canvas = document.getElementById("canvas-test");
    const context = canvas.getContext("2d");
    const size = Math.min(canvas.width, canvas.height);
    const image = new Image();

    function isBlack(r, g, b) {
        return r <= 25.65
            && g <= 30.92
            && b <= 32.08;
    }

    function isWhite(r, g, b) {
        return r >= 151.05
            && g >= 161
            && b >= 164.13;
    }

    function calculate() {
        const pixels = context.getImageData(0, 0, canvas.width, canvas.height).data;
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
                probabilities[key] = { probabilityStone: 0, probabilityBlack: 0, probabilityWhite: 0 };
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
                const stone = probabilities[key];

                stone.probabilityStone += 1;
                if (match.color === 0) {
                    stone.probabilityBlack += 1;
                }
                if (match.color === 1) {
                    stone.probabilityWhite += 1;
                }
            }
        }

        const probabilityValues = Object.values(probabilities);
        const maximumProbability = probabilityValues.reduce(function (a, b) {
            return { probabilityStone: Math.max(a.probabilityStone, b.probabilityStone) };
        });
        const board = document.getElementById("board");
        const stones = {};
        board.style = "display: flex; flex-wrap: wrap; width: 475px; float: left; background-color: #E3B579;";
        for (let y = 1; y < 20; y++) {
            const row = document.createElement("div");
            row.style.display = "flex";
            row.style.flexBasis = "100%";
            row.style.flexDirection = "row";
            for (let x = 1; x < 20; x++) {
                const stone = document.createElement("div");
                const key = y.toString() + "-" + x.toString();
                const probability = probabilities[key];
                const probabilityPercentage = probability.probabilityStone / maximumProbability.probabilityStone;
                const isBlack = probability.probabilityBlack > 0 && probability.probabilityBlack > probability.probabilityWhite;
                const isWhite = probability.probabilityWhite > 0 && probability.probabilityWhite > probability.probabilityBlack;

                stone.style.width = "25px";
                stone.style.height = "25px";
                stone.style.borderRadius = "50%";
                if (probabilityPercentage > 0.2) {
                    if (isBlack) {
                        stone.style.backgroundColor = "#2F2F2F";
                        stones[key] = { isStone: true, color: 0 };
                    }
                    if (isWhite) {
                        stone.style.backgroundColor = "#FDFDFD";
                        stones[key] = { isStone: true, color: 1 };
                    }
                }
                else {
                    stone.style.opacity = "0.2";
                    stones[key] = { isStone: false, color: -1 };
                }
                row.appendChild(stone);
            }
            board.appendChild(row);
        }
    }

    image.addEventListener("load", function () {
        canvas.width = image.width;
        canvas.height = image.height;
        canvas.style.cssFloat = "left";
        context.drawImage(image, 0, 0, image.width, image.height);

        calculate();
    });
    image.src = "board-analog-cropped.jpg";
});