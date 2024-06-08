
const awesomeIcons = {
    "volume-off": "\uf026",
    "volume-low": "\uf027",
    "volume-medium": "\uf6a8",
    "volume-high": "\uf028",
    "volume-muted": "\uf2e2",
    "microphone": "\uf130",
    "microphone-muted": "\uf131",
    "fan": "\uf863",
    "temperature": "\uf76b",
    "microchip": "\uf2db",
    "telegram": "\uf2c6",
    "dropbox": "\uf16b",
}

// style: solid, regular, thin, light
// size: gigantic, massive, hugerass, hugeass, larger, large, norm, small, smallie, smaller, tiny, poof
export function AwesomeIcon(icon, { size="norm", style="regular" }) {
    const view = Widget.Label({
        className: `icon-awesome-${style} txt-${size}`,
        label: awesomeIcons[icon] ?? icon,
    })

    return Object.assign(view, {
        setIcon(name) {
            view.label = awesomeIcons[name] ?? name
        }
    })
}

export function MaterialIcon(icon, { size="norm" }) {
    const view = Widget.Label({
        className: `icon-material txt-${size}`,
        label: icon,
    })

    return Object.assign(view, {
        setIcon(name) {
            view.label = name
        }
    })
}
