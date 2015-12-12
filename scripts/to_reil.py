"""
Parse a module in a BinNavi database and translate it to REIL code.

Author: Adrian Herrera
"""

from __future__ import print_function

import argparse
import sys


def print_error(*args):
    """
    Print an error message to stderr.

    Args:
        args: Variable-length arguments to print in the error message
    """
    print('ERROR:', *args, file=sys.stderr)


def parse_args():
    """
    Parse the command-line arguments.

    Returns:
        The parsed command-line arguments
    """
    parser = argparse.ArgumentParser(description='Convert a BinNavi module to '
                                                 'REIL code')
    parser.add_argument('-b', '--binnavi-path', action='store', required=True,
                        help='BinNavi installation directory')
    parser.add_argument('-d', '--db-description', action='store',
                        required=True,
                        help='The description of the database')
    parser.add_argument('-ho', '--db-host', action='store',
                        default='localhost',
                        help='Host address of the database server')
    parser.add_argument('-n', '--db-name', action='store', required=True,
                        help='The name of the database on the database server')
    parser.add_argument('-u', '--db-user', action='store', required=True,
                        help='The user that is used to connect to the '
                        'database')
    parser.add_argument('-p', '--db-password', action='store', required=True,
                        help='The password that is used to connect to the '
                        'database')
    parser.add_argument('-i', '--db-identity', action='store', required=True,
                        help='The identity under which the current user '
                        'operates')
    parser.add_argument('-m', '--module', action='store', required=True,
                        help='Module name')
    parser.add_argument('-o', '--output', action='store', required=True,
                        help='Output file path')

    return parser.parse_args()


def load_binnavi(path):
    """
    Include the relevant JAR files that contain the BinNavi API and the
    database access layer and import the required modules.

    Args:
        path (str): BinNavi JAR path

    Returns:
        The BinNavi plugin interface that can be used in standalone scripts, or
        `None` if BinNavi fails to load
    """
    sys.path.append(path)

    try:
        from com.google.security.zynamics.binnavi.API.plugins import StandAlone

        return StandAlone.getPluginInterface()
    except ImportError:
        return None


def get_database(plugin_interface, db_info):
    """
    Get the database based on the given description.

    Args:
        plugin_interface (PluginInterface): The BinNavi plugin interface
        db_info (dict): A dictionary that describes the database connection

    Returns:
        The disconnected and unloaded database, or `None` if it doesn't exist
    """
    dbm = plugin_interface.getDatabaseManager()

    try:
        return dbm.addDatabase(db_info['description'],
                               'org.postgresql.Driver',
                               db_info['host'],
                               db_info['name'],
                               db_info['user'],
                               db_info['password'],
                               db_info['identity'],
                               False,
                               False)
    except:
        return None


def get_module(database, module_name):
    """
    Get the module with the given name.

    Args:
        db (Database): The connected and loaded database
        module_name (str): The module name

    Returns:
        The unloaded module, or `None` if it doesn't exist
    """
    for module in database.getModules():
        if module.getName() == module_name:
            return module

    return None


def get_functions(module):
    """
    Get a list of functions from the given module.

    Args:
        module (Module): The loaded module

    Returns:
        A list of unloaded functions from the loaded module
    """
    from com.google.security.zynamics.binnavi.API.disassembly import \
        FunctionType

    # We are not interested in functions that are dynamically imported from an
    # external module
    return [func for func in module.getFunctions()
            if func.getType() != FunctionType.Import]


def get_reil(function):
    """
    Get the REIL code for a given function.

    Args:
        function (Function): The function

    Returns:
        A tuple of the function's start address and a list of REIL nodes,
        sorted by address
    """
    function.load()
    reil_nodes = [node for node in
                 function.getReilCode().getGraph().getNodes()]
    sorted(reil_nodes, key=lambda n: n.getAddress())
    function.close()
    print('Translated function `%s`' % function.getName())

    return reil_nodes[0].getAddress(), reil_nodes


def main():
    """
    The main function.
    """
    # Parse the command-line arguments
    args = parse_args()

    # Load the BinNavi plugin interface
    plugin_interface = load_binnavi(args.binnavi_path)
    if plugin_interface is None:
        print_error('Unable to load BinNavi from `%s`' % args.binnavi_path)
        sys.exit(1)

    # Connect to and load the required database
    db_info = {
        'description': args.db_description,
        'host': args.db_host,
        'name': args.db_name,
        'user': args.db_user,
        'password': args.db_password,
        'identity': args.db_identity,
    }

    database = get_database(plugin_interface, db_info)
    if database is None:
        print_error('Unable to load database `%s`' % db_info['name'])
        sys.exit(1)
    database.connect()
    database.load()

    # Load the required module
    module = get_module(database, args.module)
    if module is None:
        module_names = ['`%s`' % mod.getName() for
                        mod in database.getModules()]
        print_error('Unable to load module `%s`. Available modules are %s' % \
            (args.module, ', '.join(module_names)))
        sys.exit(1)
    module.load()

    # Get a list of tuples that contain the start address and the REIL code for
    # a particular function
    functions = get_functions(module)
    reil_functions = [get_reil(func) for func in functions]

    # Sort the functions by their start address (the first element in the
    # tuple)
    sorted(reil_functions, key=lambda f: f[0])

    # Write translated REIL code to the output file
    reil_str = '\n'.join([str(node).strip() for _, func in reil_functions
                                            for node in func])
    with open(args.output, 'w') as out_file:
        out_file.write(reil_str)

    # Clean up
    module.close()
    database.close()

    print('REIL code for `%s` successfully written to `%s`' % \
        (args.module, args.output))


if __name__ == '__main__':
    main()
