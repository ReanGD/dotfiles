from icon import Icon
from writer import Writer
from dataclasses import dataclass


@dataclass
class Storage:
    writer: Writer
    icon_resolver: Icon
