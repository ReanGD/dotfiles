# -*- coding: utf-8 -*-
from ansible.errors import *


class LookupModule:
    def __init__(self, **kwargs):
        pass

    def __parse_term(self, term):
        params = term.split()
        config_path = params[0]
        paramvals = {
            'action': "all_pkg",
        }
        avaible_actions = ['all_pkg', 'not_installed']

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

    def run(self, terms, variables=None, **kwargs):
        params = self.__parse_term(terms)
        return ["python2-idna", "python2-notify"]
