const Audio = await Service.import("audio")
import { AwesomeIcon } from "../widgets/icon.js";

function Volume() {
    function getValue() {
        if (!Audio.speaker || Audio.speaker.is_muted) {
            return "0"
        }

        return `${Math.round(Audio.speaker.volume * 100)}%`
    }

    function incValue(dt) {
        if (!Audio.speaker) return;
        Audio.speaker.volume += dt;
        // Indicator.popup(1);
    }

    function getIconName() {
        if (!Audio.speaker || Audio.speaker.is_muted) {
            return "volume-muted"
        }
        const volume = Audio.speaker.volume * 100
        if (volume >= 67) {
            return "volume-high"
        }
        if (volume >= 34) {
            return "volume-medium"
        }
        if (volume >= 1) {
            return "volume-low"
        }
        return "volume-off"
    }

    const icon = AwesomeIcon(getIconName(), {
        size: "smallie",
        style: "regular",
    })

    const value = Widget.Label(getValue())

    return Widget.Button({
        onScrollUp: () => incValue(0.004),
        onScrollDown: () => incValue(-0.004),
        setup: (self) => self.hook(Audio, () => {
            icon.setIcon(getIconName())
            value.label = getValue()
        }, "speaker-changed"),
        cursor: "pointer",
        child: Widget.Box({
            spacing: 8,
            class_name: "volume",
            children: [icon, value]
        }),
    })
}

function Microphone() {
    function getValue() {
        if (!Audio.microphone || Audio.microphone.is_muted) {
            return "0"
        }

        return `${Math.round(Audio.microphone.volume * 100)}%`
    }

    function incValue(dt) {
        if (!Audio.microphone) return;
        Audio.microphone.volume += dt;
        // Indicator.popup(1);
    }

    function getIconName() {
        if (!Audio.microphone || Audio.microphone.is_muted) {
            return "microphone-muted"
        }
        return "microphone"
    }

    const icon = AwesomeIcon(getIconName(), {
        size: "smallie",
        style: "regular",
    })

    const value = Widget.Label(getValue())

    return Widget.Button({
        onScrollUp: () => incValue(0.004),
        onScrollDown: () => incValue(-0.004),
        setup: (self) => self.hook(Audio, () => {
            icon.setIcon(getIconName())
            value.label = getValue()
        }, "microphone-changed"),
        cursor: "pointer",
        child: Widget.Box({
            spacing: 8,
            class_name: "microphone",
            children: [icon, value]
        }),
    })
}

export default () => {
    return Widget.Box({
        class_name: "pulseaudio",
        children: [Volume(), Microphone()]
    });
}
