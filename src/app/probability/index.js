document.addEventListener("DOMContentLoaded", function () {
    const canvas = document.getElementById("canvas-output");
    const context = canvas.getContext("2d");
    const size = Math.min(canvas.width, canvas.height);
    const image = new Image();

    function isBlack(r, g, b) {
        return r <= 35
            && g <= 35
            && b <= 35;
    }

    function isWhite(r, g, b) {
        return r >= 150
            && g >= 150
            && b >= 150;
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

        const probabilities = [];
        for (let i = 0; i < 362; i++) {
            probabilities[i] = { probabilityStone: 0, probabilityBlack: 0, probabilityWhite: 0 };
        }

        for (let i = 0; i < matches.length; i++) {
            const match = matches[i];
            const percentageX = match.x / canvas.width;
            const percentageY = match.y / canvas.height;
            const x = Math.round(19 * percentageX);
            const y = Math.round(19 * percentageY);

            if (x > 0 && y > 0) {
                const stone = probabilities[x * y];

                stone.probabilityStone += 1;
                if (match.color === 0) {
                    stone.probabilityBlack += 1;
                }
                if (match.color === 1) {
                    stone.probabilityWhite += 1;
                }
            }
        }

        console.log(probabilities);

        const board = document.getElementById("board");
        board.style = "display: flex; flex-wrap: wrap; width: 475px; background-color: #E3B579;";
        for (let y = 1; y < 20; y++) {
            const row = document.createElement("div");
            row.style.display = "flex";
            row.style.flexBasis = "100%";
            row.style.flexDirection = "row";
            for (let x = 1; x < 20; x++) {
                const stone = document.createElement("div");
                const probability = probabilities[(x * y) - 1];
                const isBlack = probability.probabilityBlack > 0;
                const isWhite = probability.probabilityWhite > 0;

                stone.style.width = "25px";
                stone.style.height = "25px";
                stone.style.borderRadius = "50%";
                if (isBlack && isWhite) {
                    stone.style.backgroundColor = "#F44336";
                }
                else {
                    if (isBlack) {
                        stone.style.backgroundColor = "#2F2F2F";
                    }
                    if (isWhite) {
                        stone.style.backgroundColor = "#FDFDFD";
                    }
                }
                row.appendChild(stone);
            }
            board.appendChild(row);
        }
    }

    image.addEventListener("load", function () {
        canvas.width = image.width;
        canvas.height = image.height;
        context.drawImage(image, 0, 0, image.width, image.height);

        calculate();
    });
    image.src = "board-analog-cropped.jpg";
});