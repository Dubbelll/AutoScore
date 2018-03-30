addEventListener("message", function (event) {
    const size = event.data.size;
    const matches = event.data.matches;
    const board = [];

    for (let i = 1; i < 361; i++) {
        board[i] = { probabilityStone: 0, probabilityBlack: 0, probabilityWhite: 0 };
    }

    for (let i = 0; i < matches.length; i++) {
        const match = matches[i];
        const percentageX = match.x / size;
        const percentageY = match.y / size;
        const x = Math.round(19 * percentageX);
        const y = Math.round(19 * percentageY);

        if (x > 0 && y > 0) {
            const stone = board[x * y];

            stone.probabilityStone += 1;
            if (match.color === 0) {
                stone.probabilityBlack += 1;
            }
            if (match.color === 1) {
                stone.probabilityWhite += 1;
            }
        }
    }

    console.log(board);

    this.postMessage(true);

    /* const matches = event.data.matches;
    const size = event.data.size;
    const minimumGroupSize = (size * size) * 0.5;
    const maximumGroupSize = (size * size) * 1.25;

    function isWithinRange(a, b, range) {
        return (a.x === b.x || a.x - range === b.x || a.x + range === b.x)
            && (a.y === b.y || a.y - range === b.y || a.y + range === b.y)
            && a.color === b.color;
    }

    function groupMatches(groups, matches, matchIndex) {
        const match = matches[matchIndex];
        const exisitingGroupIndexes = [];

        loopGroups:
        for (let i = 0; i < groups.length; i++) {
            const group = groups[i];

            loopMembers:
            for (let j = 0; j < group.length; j++) {
                const member = group[j];

                if (isWithinRange(match, member, 1)) {
                    exisitingGroupIndexes.push(i);

                    break loopGroups;
                }
            }
        }

        if (exisitingGroupIndexes.length === 0) {
            groups.push([match]);
        }
        if (exisitingGroupIndexes.length === 1) {
            const groupIndex = exisitingGroupIndexes[0];
            groups[groupIndex].push(match);
        }

        return groups;
    }

    let groups = [];
    console.log("start grouping: " + new Date().toUTCString())
    for (let i = 0; i < matches.length; i++) {
        groups = groupMatches(groups, matches, i);
    }
    console.log("end grouping: " + new Date().toUTCString());

    const filtered = groups.filter(function (group) {
        return group.length >= minimumGroupSize && group.length <= maximumGroupSize;
    });
    const tooBig = groups.filter(function (group) {
        return group.length > maximumGroupSize;
    });
    console.log(groups.length);
    console.log(filtered.length);
    console.log(tooBig.length);

    postMessage(true); */
});