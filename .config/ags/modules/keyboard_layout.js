const hyprland = await Service.import("hyprland")

export default () => {
    function getLayoutName(layoutName) {
        if (layoutName.includes("English")) {
            return "US"
        }
        if (layoutName.includes("Russian")) {
            return "RU"
        }
        return layoutName
    }

    const devices = Utils.exec("hyprctl devices -j")
    const { keyboards } = JSON.parse(devices)
    const mainKb = keyboards.find(({ main }) => main === true)
    const device = mainKb?.name ?? ""
    var initLayoutName = getLayoutName(mainKb?.active_keymap ?? "None")

    const view = Widget.Label({
        class_name: "keyboard-layout",
        label: initLayoutName,
    })

    return Widget.Button({
        cursor: "pointer",
        child: view,
        onClicked: () => hyprland.messageAsync("switchxkblayout " + device + " next"),
        setup: (self) => self.hook(hyprland, (self, kbName, layoutName) => {
            view.label = getLayoutName(layoutName ?? initLayoutName)
            initLayoutName = "None"
        }, "keyboard-layout")
    });
}
