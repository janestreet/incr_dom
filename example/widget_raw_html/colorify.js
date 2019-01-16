var colors = ["red", "green", "blue", "yellow", "orange"];
window.colorify = function (dom_node) {
  dom_node.onclick = function () {
    var i = Math.floor(Math.random() * colors.length);
    var foreground = colors[i];
    var background = colors[(i + 1) % colors.length];
    dom_node.style.color = foreground;
    dom_node.style.backgroundColor = background;
  };
};
