const { GLib } = imports.gi;

export default () => {
    const view = Widget.Label({
        class_name: "clock",
    })

    const main_format = "%a %d.%m • %H:%M"
    const tooltip_format = "%A %d %B %Y"

    function update() {
        const main_time = GLib.DateTime.new_now_local().format(main_format) ?? "unknown time"
        const tooltip_time = GLib.DateTime.new_now_local().format(tooltip_format) ?? "unknown time"
        view.label = main_time
        view.tooltip_text = tooltip_time
    }

    return Widget.EventBox({
        child: view,
        setup: (self) => {
            update()
            self.poll(5000, update)
        }
    });
}
