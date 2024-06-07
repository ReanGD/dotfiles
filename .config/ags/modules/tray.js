const systemtray = await Service.import("systemtray")

const SysTrayItem = (item) => Widget.Button({
    className: "bar-systray-item",
    child: Widget.Icon({hpack: "center"}).bind("icon", item, "icon"),
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
