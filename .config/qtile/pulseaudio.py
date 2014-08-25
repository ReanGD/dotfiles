from libqtile.widget import base
import subprocess


class PulseAudio(base.InLoopPollText):
    """
        Widget that display and change volume (use pulseaudio)
    """
    defaults = [
        ('format', 'vol:{0}%', 'Volume format string.'),
        ('update_interval', 1., 'Update interval in seconds.'),
    ]

    NAME = "pulseaudio"

    def __init__(self, **config):
        config["name"] = PulseAudio.NAME
        base.InLoopPollText.__init__(self, **config)
        self.add_defaults(PulseAudio.defaults)

    def get_sink_info(self):
        try:
            dump_data = subprocess.check_output(["pacmd", "dump"]).split("\n")
        except subprocess.CalledProcessError:
            return (None, None)

        def_sink = filter(lambda l: l.startswith("set-default-sink"),
                          dump_data)[0].split()[1]
        vol_find = "set-sink-volume " + def_sink
        volume = int(filter(lambda l: l.startswith(vol_find),
                            dump_data)[0].split()[-1], 16)
        return (def_sink, volume)

    def add_volume(self, add_percent):
        sink_name, cur_volume = self.get_sink_info()
        if sink_name is not None:
            max_volume = 300*0x10000
            new_volume = cur_volume*100 + add_percent*0x10000
            str_volume = str(min(max(new_volume, 0), max_volume)/100)
            subprocess.call(["pacmd", "set-sink-volume", sink_name, str_volume])
            self.tick()

    def poll(self):
        sink_name, cur_volume = self.get_sink_info()
        if sink_name is not None:
            cur_volume = int(round(float(cur_volume)*100.0/0x10000))
            return self.format.format(cur_volume)
        else:
            return self.text


def inc_volume(qtile):
    w = qtile.widgetMap.get(PulseAudio.NAME)
    w.add_volume(10)


def dec_volume(qtile):
    w = qtile.widgetMap.get(PulseAudio.NAME)
    w.add_volume(-10)
