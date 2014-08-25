from libqtile.widget import base


class Metrics(base.InLoopPollText):
    """
        Widget that display memory and cpu usage
    """
    defaults = [
        ('mem_format', 'mem:{0}%', 'Memory format string.'),
        ('cpu_format', 'cpu:{0}%', 'CPU format string.'),
        ('update_interval', 1., 'Update interval in seconds.'),
    ]

    def __init__(self, **config):
        base.InLoopPollText.__init__(self, **config)
        self.add_defaults(Metrics.defaults)
        self.old_cpu_usage = (0, 0)

    def get_cpu_usage(self):
        # name, user, nice, sys, idle, iowait, irq, softirq
        stat = [int(it) for it in open('/proc/stat').readline().split()[1:]]
        return sum(stat[:3]), sum(stat)

    def get_cpu_info(self):
        ncpu_usage, ncpu_total = self.get_cpu_usage()
        cpu_usage = ncpu_usage - self.old_cpu_usage[0]
        cpu_total = ncpu_total - self.old_cpu_usage[1]
        if cpu_total != 0 and self.old_cpu_usage[0] != 0:
            cpu_percent = int(float(cpu_usage) / float(cpu_total) * 100)
        else:
            cpu_percent = 'nan'

        self.old_cpu_usage = (ncpu_usage, ncpu_total)
        return self.cpu_format.format(cpu_percent)

    def get_mem_info(self):
        info = {}
        for line in open('/proc/meminfo'):
            key, val = line.split(':')
            info[key] = int(val.split()[0])
        mem = info['MemTotal']-info['MemFree']-info['Buffers']-info['Cached']
        if int(info['MemTotal']) != 0:
            mem_percents = int(float(mem) / float(info['MemTotal']) * 100)
        else:
            mem_percents = 'nan'
        return self.mem_format.format(mem_percents)

    def poll(self):
        cpu_info = self.get_cpu_info()
        mem_info = self.get_mem_info()
        return cpu_info + " " + mem_info
