import Service from "resource:///com/github/Aylur/ags/service.js";


class NvidiaService extends Service {
    static {
        Service.register(
            this,
            {},
            {
                "fanSpeed": ["int", "r"],
                "temperature": ["int", "r"],
                "memoryTotal": ["int", "r"],
                "memoryUsed": ["int", "r"],
                "tooltip": ["string", "r"],
            },
        );
    }

    #available = false
    #fanSpeed = 0
    #temperature = 0
    #memoryTotal = 0
    #memoryUsed = 0
    #tooltip = ""

    get available() {
        return this.#available
    }

    get fanSpeed() {
        return this.#fanSpeed
    }

    get temperature() {
        return this.#temperature
    }

    get memoryTotal() {
        return this.#memoryTotal
    }

    get memoryUsed() {
        return this.#memoryUsed
    }

    get tooltip() {
        return this.#tooltip
    }

    constructor() {
        super();
        const data = Utils.exec("lspci")
        if (data.includes("NVIDIA")) {
            this.#available = true
        }
        Utils.interval(1000, () => {
            Utils.execAsync("nvidia-smi --query-gpu=temperature.gpu,fan.speed,memory.total,memory.used --format=csv,noheader,nounits")
                .then(output => this.#onChange(output))
        })
    }

    #onChange(output) {
        const [temperatureStr, fanSpeedStr, memoryTotalStr, memoryUsedStr] = output.split(",")

        var changed = false
        const temperature = Number(temperatureStr)
        if (this.#temperature !== temperature) {
            changed = true
            this.#temperature = temperature
            this.notify("temperature")
        }

        const fanSpeed = Number(fanSpeedStr)
        if (this.#fanSpeed !== fanSpeed) {
            changed = true
            this.#fanSpeed = fanSpeed
            this.notify("fanSpeed")
        }

        const memoryTotal = Number(memoryTotalStr)
        if (this.#memoryTotal !== memoryTotal) {
            changed = true
            this.#memoryTotal = memoryTotal
            this.notify("memoryTotal")
        }

        const memoryUsed = Number(memoryUsedStr)
        if (this.#memoryUsed !== memoryUsed) {
            changed = true
            this.#memoryUsed = memoryUsed
            this.notify("memoryUsed")
        }

        if (changed) {
            this.#tooltip = `GPU Temperature: ${temperature}°C\nFan Speed: ${fanSpeed}%\nMemory: ${memoryUsed} / ${memoryTotal} MiB`
            this.notify("tooltip")
            this.emit("changed")
        }
    }
}

const service = new NvidiaService()

export default service
