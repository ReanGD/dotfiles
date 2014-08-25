import os
from libqtile import bar
from libqtile.widget import base

BAT_DIR = '/sys/class/power_supply'
CHARGED = 'Full'
CHARGING = 'Charging'
DISCHARGING = 'Discharging'
UNKNOWN = 'Unknown'

BATTERY_INFO_FILES = {
    'energy_now_file': ['energy_now', 'charge_now'],
    'energy_full_file': ['energy_full', 'charge_full'],
    'power_now_file': ['power_now', 'current_now'],
    'status_file': ['status'],
}


class _Battery(base._TextBox):
    ''' Base battery class '''

    filenames = {}

    defaults = [
        ('battery_name', 'BAT0', 'ACPI name of a battery, usually BAT0'),
        (
            'status_file',
            'status',
            'Name of status file in'
            ' /sys/class/power_supply/battery_name'
        ),
        (
            'energy_now_file',
            None,
            'Name of file with the '
            'current energy in /sys/class/power_supply/battery_name'
        ),
        (
            'energy_full_file',
            None,
            'Name of file with the maximum'
            ' energy in /sys/class/power_supply/battery_name'
        ),
        (
            'power_now_file',
            None,
            'Name of file with the current'
            ' power draw in /sys/class/power_supply/battery_name'
        ),
        ('update_delay', 1, 'The delay in seconds between updates'),
    ]

    def __init__(self, **config):
        # base._TextBox.__init__(self, "BAT", bar.CALCULATED, **config)
        base._TextBox.__init__(self, "", bar.CALCULATED, **config)
        self.add_defaults(_Battery.defaults)

    def _load_file(self, name):
        try:
            path = os.path.join(BAT_DIR, self.battery_name, name)
            with open(path, 'r') as f:
                return f.read().strip()
        except IOError:
            if name == 'current_now':
                return 0
            return False
        except Exception:
            self.log.exception("Failed to get %s" % name)

    def _get_param(self, name):
        if name in self.filenames:
            return self._load_file(self.filenames[name])
        else:
            ## Don't have the file name cached, figure it out
            file_list = BATTERY_INFO_FILES.get(name, [])
            if getattr(self, name, None):
                ## If a file is manually specified, check it first
                file_list.insert(0, getattr(self, name))

            ## Iterate over the possibilities, and return the first valid value
            for file in file_list:
                value = self._load_file(file)
                if not (value in (False, None)):
                    self.filenames[name] = file
                    return value

        ## If we made it this far, we don't have a valid file. Just return None.
        return None

    def _get_info(self):
        try:
            info = {
                'stat': self._get_param('status_file'),
                'now': float(self._get_param('energy_now_file')),
                'full': float(self._get_param('energy_full_file')),
                'power': float(self._get_param('power_now_file')),
            }
        except TypeError:
            return False
        return info


class Battery(_Battery):
    """
        A simple but flexible text-based battery widget.
    """
    defaults = [
        ('low_foreground', 'FF0000', 'font color when battery is low'),
        (
            'format',
            '{char} {percent:2.0%} {hour:d}:{min:02d}',
            'Display format'
        ),
        ('charge_char', '^', 'Character to indicate the battery is charging'),
        (
            'discharge_char',
            'V',
            'Character to indicate the battery'
            ' is discharging'
        ),
        (
            'low_percentage',
            0.10,
            "0 < x < 1 at which to indicate battery is low with low_foreground"
        ),
        ('hide_threshold', None, 'Hide the text when there is enough energy'),
    ]

    def __init__(self, **config):
        _Battery.__init__(self, **config)
        self.add_defaults(Battery.defaults)
        self.timeout_add(self.update_delay, self.update)
        self.update()

    def _get_text(self):
        info = self._get_info()
        if info is False:
            return None

        ## Set the charging character
        try:
            # hide the text when it's higher than threshold, but still
            # display `full` when the battery is fully charged.
            if self.hide_threshold and \
                    info['now'] / info['full'] * 100.0 >= \
                    self.hide_threshold and \
                    info['stat'] != CHARGED:
                return ''
            elif info['stat'] == DISCHARGING:
                char = self.discharge_char
                time = info['now'] / info['power']
            elif info['stat'] == CHARGING:
                char = self.charge_char
                time = (info['full'] - info['now']) / info['power']
            else:
                return 'Full'
        except ZeroDivisionError:
            time = -1

        ## Calculate the battery percentage and time left
        if time >= 0:
            hour = int(time)
            min = int(time * 60) % 60
        else:
            hour = -1
            min = -1
        percent = info['now'] / info['full']
        if info['stat'] == DISCHARGING and percent < self.low_percentage:
            self.layout.colour = self.low_foreground
        else:
            self.layout.colour = self.foreground

        return self.format.format(
            char=char,
            percent=percent,
            hour=hour,
            min=min
        )

    def update(self):
        if self.configured:
            ntext = self._get_text()
            if ntext is not None and ntext != self.text:
                self.text = ntext
                self.bar.draw()
        return True
