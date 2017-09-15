from ansible.module_utils.basic import AnsibleModule

import socket
import xmlrpclib
from supervisor.xmlrpc import SupervisorTransport
from supervisor.options import ServerOptions


def main():
    module = AnsibleModule(
        argument_spec = dict(
            config=dict(required=True, type='path'),
            state=dict(default='present', choices=['present', 'absent'])
        ),
        supports_check_mode=True
    )

    configfile = module.params['config']

    options = ServerOptions()
    options.configfile = configfile
    options.progname = 'supervisord'

    try:
        options.process_config(do_usage=True)
    except ValueError as e:
        module.fail_json(msg = e.message.message)

    options.realize(args=[], progname='supervisord')

    server = xmlrpclib.ServerProxy(
        'http://127.0.0.1',
        allow_none=True,
        transport=SupervisorTransport(serverurl=options.serverurl))

    try:
        server.supervisor.getPID()
        state = 'present'
    except socket.error:
        state = 'absent'

    desired = module.params['state']

    if state == desired:
        module.exit_json(changed=False)

    if not module.check_mode:
        if desired == 'present':
            module.run_command(["supervisord", "-c", configfile], check_rc=True)
        elif desired == 'absent':
            server.supervisor.shutdown()

    module.exit_json(changed=True)


if __name__ == '__main__':
    main()
