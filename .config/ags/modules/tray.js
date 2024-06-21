const systemtray = await Service.import("systemtray")
import { AwesomeIcon } from "../widgets/icon.js";

function TrayIcon(item) {
    if (item.icon === "nm-device-wired") {
        return AwesomeIcon("network-wired", {
            size: "smallie",
            style: "regular",
        })
    }

    return Widget.Icon({
        className: `txt-norm`,
        hpack: "center"
    }).bind("icon", item, "icon")
}

const SysTrayItem = (item) => Widget.Button({
    className: "bar-systray-item",
    child: TrayIcon(item),
    tooltip_markup: item.bind("tooltip_markup"),
    onPrimaryClick: (_, event) => item.activate(event),
    onSecondaryClick: (_, event) => item.openMenu(event),
});

export default () => {
    return Widget.Box({
        spacing: 8,
        class_name: "tray",
        setup: (self) => self
            .hook(systemtray, (self) => {
                self.children = systemtray.items.map(SysTrayItem);
            })
        ,
    })
}
