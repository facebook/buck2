use std::path::PathBuf;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;

use buck2_forkserver::client::ForkserverClient;
use buck2_forkserver::run::GatherOutputStatus;
use clap::Parser;
use futures::stream;
use futures::stream::TryStreamExt;
use futures::StreamExt;
use tokio::io::AsyncWriteExt;

#[derive(Parser)]
struct Opt {
    #[clap(long, help = "path to the Buck2 CLI")]
    buck2: PathBuf,

    #[clap(
        long,
        default_value = "1",
        help = "How many times to repeat the command"
    )]
    repeat: usize,

    #[clap(
        long,
        default_value = "1",
        help = "How many instances of the command to run concurrently"
    )]
    concurrency: usize,

    #[clap(long)]
    no_stdout: bool,

    #[clap(long)]
    no_stderr: bool,

    #[clap()]
    exe: String,

    #[clap()]
    args: Vec<String>,
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let Opt {
        buck2,
        repeat,
        concurrency,
        no_stdout,
        no_stderr,
        exe,
        args,
    } = Opt::from_args();

    let forkserver: anyhow::Result<ForkserverClient>;

    #[cfg(unix)]
    {
        forkserver = buck2_forkserver::unix::launch_forkserver(&buck2, &["forkserver"]).await;
    }
    #[cfg(not(unix))]
    {
        forkserver = Err(anyhow::anyhow!("This is UNIX only"));
    }

    let forkserver = forkserver?;

    let req = buck2_forkserver_proto::CommandRequest {
        exe: exe.into_bytes(),
        argv: args.into_iter().map(|a| a.into_bytes()).collect(),
        ..Default::default()
    };

    let failures = AtomicUsize::new(0);

    stream::iter(0..repeat)
        .map(Ok)
        .try_for_each_concurrent(concurrency, |_i| async {
            let (status, out, err) = forkserver
                .execute(req.clone(), futures::future::pending())
                .await?;
            if !matches!(status, GatherOutputStatus::Finished(s) if s.success()) {
                failures.fetch_add(1, Ordering::Relaxed);
            }
            if !no_stdout {
                tokio::io::stdout().write_all(&out).await?;
            }
            if !no_stderr {
                tokio::io::stderr().write_all(&err).await?;
            }
            anyhow::Ok(())
        })
        .await?;

    let failures = failures.load(Ordering::Relaxed);
    if failures > 0 {
        return Err(anyhow::anyhow!("{} failures", failures));
    }

    Ok(())
}
