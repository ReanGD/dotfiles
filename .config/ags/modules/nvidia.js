import Nvidia from "../services/nvidia.js";
import { AwesomeIcon } from "../widgets/icon.js";

export default () => {
    if (!Nvidia.available) {
        return null;
    }
    return Widget.Box({
        spacing: 8,
        class_name: "nvidia",
        tooltip_text: Nvidia.bind("tooltip"),
        children: [
            AwesomeIcon("temperature", {
                size: "smallie",
                style: "regular",
            }),
            Widget.Label({
                label: Nvidia.bind("temperature").as(value => `${value}°C`),
            }),
            AwesomeIcon("fan", {
                size: "smallie",
                style: "regular",
            }),
            Widget.Label({
                label: Nvidia.bind("fanSpeed").as(value => `${value}%`),
            }),
        ],
    })
}
