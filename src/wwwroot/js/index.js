window.circles = {
    getPos: function (elt) {
        const rect = elt.getBoundingClientRect();
        return [rect.x, rect.y];
    }
};
