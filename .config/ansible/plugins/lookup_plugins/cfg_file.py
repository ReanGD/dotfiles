# -*- coding: utf-8 -*-
import sys
import os.path
from ansible.errors import *


class UserConfig:
    def __init__(self, config_path):
        self.config_data = {}
        full_path = os.path.expanduser(config_path)
        if not os.path.exists(full_path):
            raise AnsibleError('config file "%s" not found' % full_path)
        try:
            execfile(os.path.expanduser(full_path), self.config_data)
        except:
            e = sys.exc_info()[1]
            raise AnsibleError(e)

    def __getitem__(self, item):
        return set(self.config_data[item])


class LookupModule:
    def __init__(self, **kwargs):
        self.cfg = None

    def __parse_term(self, terms):
        params = terms.split()
        config_path = params[0]
        paramvals = {
            'action': "all",
        }
        avaible_actions = ['all', 'not_installed']

        try:
            for param in params[1:]:
                name, value = param.split('=')
                assert(name in paramvals)
                paramvals[name] = value
            assert(paramvals['action'] in avaible_actions)
        except (ValueError, AssertionError) as e:
            raise AnsibleError(e)

        paramvals['path'] = config_path
        return paramvals

    def __all(self):
        return []

    def __not_installed(self):
        return []

    def run(self, terms, variables=None, **kwargs):
        params = self.__parse_terms(terms)
        action = params['action']
        self.cfg = UserConfig(params['path'])

        if action == "all":
            return self.__all()
        elif action == "not_installed":
            return self.__not_installed()
        else:
            raise AnsibleError("error action (%s)" % action)
