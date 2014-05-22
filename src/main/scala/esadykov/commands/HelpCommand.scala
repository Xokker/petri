package esadykov.commands

/**
 * @author Ernest Sadykov
 * @since 22.05.2014
 */
object HelpCommand extends Command {
    val HelpPrompt =
        """
          |That application allows you to work with workflow modules.
          |Two or more nets can be connected using 'connect' command.
          |One (and only one) of the sockets must be input socket.
          |
          |type 'exit' for quit the application
          |type 'connect 1.A 3.Dc' for connect socket A of the net 1 with socket Dc of the net 3
          |type 'connect 1.A&D 2.C 3.B' for connect sockets A and D of net 1, and socket C of net 2 with socket B of net 3.
        """.stripMargin
}
