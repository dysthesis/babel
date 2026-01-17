use serenity::{
    Client,
    all::{
        Context, CreateCommand, CreateCommandOption, CreateInteractionResponse,
        CreateInteractionResponseMessage, EventHandler, GatewayIntents, GuildId, Interaction,
        Ready,
    },
    async_trait,
};
use tracing::{error, info, warn};

use crate::query::Query;

pub struct Bot {
    client: Client,
}

#[derive(Clone, Default)]
pub struct Handler {
    guild_id: GuildId,
}

impl Handler {
    fn new(guild_id: u64) -> Self {
        let guild_id = GuildId::new(guild_id);
        Self { guild_id }
    }
}

#[async_trait]
impl EventHandler for Handler {
    async fn ready(&self, ctx: Context, ready: Ready) {
        info!("{} is connected!", ready.user.name);

        info!("Constructing `discuss` command...");
        let cmd = CreateCommand::new("discuss")
            .description("Discuss a book")
            .add_option(
                CreateCommandOption::new(
                    serenity::all::CommandOptionType::String,
                    "input",
                    "Identifier or query",
                )
                .required(true),
            );

        info!("Successfully constructed command!");

        info!("Registering commands...");
        if let Err(why) = self.guild_id.set_commands(&ctx, vec![cmd]).await {
            error!("Error registering commands: {why:?}");
        } else {
            info!("Successfully registered commands!");
        }
    }

    async fn interaction_create(&self, ctx: Context, interaction: Interaction) {
        let Interaction::Command(command) = interaction else {
            warn!("Interaction is not a command, not supported: {interaction:?}");
            return;
        };

        if command.data.name.as_str() == "discuss" {
            info!("Received `discuss` command!");

            let input = command
                .data
                .options
                .iter()
                .find(|o| o.name == "input")
                .and_then(|o| o.value.as_str())
                .unwrap_or("");
            info!("`discuss` command with input: {input}");

            let output = format!("{:?}", Query::parse(input));

            info!(
                "`discuss` command with input: {input} results in output {output:?}. Constructing response..."
            );

            let resp = CreateInteractionResponse::Message(
                CreateInteractionResponseMessage::new().content(output),
            );

            info!("Constructed response: {resp:?}");

            if let Err(why) = command.create_response(&ctx.http, resp).await {
                error!("Error responding to interaction: {why:?}");
            }
        } else {
            info!("Unsupported command: {}", command.data.name);
        }
    }
}

impl Bot {
    pub async fn new(token: String, guild_id: u64) -> Self {
        let handler = Handler::new(guild_id);
        let client = Client::builder(token, Bot::intents())
            .event_handler(handler.clone())
            .await
            .expect("Error creating client");

        info!("Guild ID is {guild_id:?}");
        Self { client }
    }

    #[inline]
    pub async fn start(&mut self) -> serenity::Result<()> {
        self.client.start().await
    }

    fn intents() -> GatewayIntents {
        GatewayIntents::GUILD_MESSAGES
            | GatewayIntents::DIRECT_MESSAGES
            | GatewayIntents::MESSAGE_CONTENT
    }
}
