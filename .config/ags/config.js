import Tray from "./modules/tray.js";
import Clock from "./modules/clock.js";
import Nvidia from "./modules/nvidia.js";
import Workspaces from "./modules/workspaces.js";
import PulseAudio from "./modules/pulseaudio.js";
import KeyboardLayout from "./modules/keyboard_layout.js";


function Left() {
    return Widget.Box({
        children: [
            Workspaces(),
            KeyboardLayout(),
        ],
    })
}

function Center() {
    return Widget.Box({
        children: [
            Clock(),
        ],
    })
}

function Right() {
    return Widget.Box({
        hpack: "end",
        spacing: 8,
        children: [
            PulseAudio(),
            Nvidia(),
            Tray()
        ]
    })
}

function Bar(monitor = 0) {
    return Widget.Window({
        name: `bar-${monitor}`, // name has to be unique
        class_name: "bar",
        monitor,
        anchor: ["top", "left", "right"],
        exclusivity: "exclusive",
        child: Widget.CenterBox({
            start_widget: Left(),
            center_widget: Center(),
            end_widget: Right(),
        }),
    })
}

App.config({
    style: "./main.css",
    windows: [
        Bar(),
        // you can call it, for each monitor
        // Bar(0),
        // Bar(1)
    ],
})

export { }
