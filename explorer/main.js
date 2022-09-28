const { app, BrowserWindow, dialog, ipcMain } = require('electron')
const isDev = require('electron-is-dev');
const util = require('util');
const execFile = util.promisify(require('child_process').execFile);

if (isDev) {
    // Reload if the client side files change
    require('electron-reload')(__dirname, {
        electron: require(`${__dirname}/node_modules/electron`)
    })
}

// The main window which is the UI
let mainWindow;

// The directory all buck2 commands are run from.
// TODO: We might want better detection or user persistence in future.
let buckDir = `${app.getPath('home')}/fbsource`;

function createWindow () {
    mainWindow = new BrowserWindow({
        width: 800,
        height: 600,
        title: "Buck2 Explorer",
        icon: `${__dirname}/icon.png`,
        webPreferences: {
            preload: `${__dirname}/preload.js`
        }
    })
    if (isDev) {
        mainWindow.webContents.openDevTools();
    }
    mainWindow.loadFile(`${__dirname}/index.html`)
}

// Throw an error if the target does not match a narrow range of permissible values.
// Make sure that anything that is a target won't be a command line escape.
function validateTarget(target) {
    if (!/[-_/+@=.A-Za-z0-9]+/.test(target)) {
        throw `Target is not valid, ${target}`;
    }
}

async function runCommand(args, host, mode) {
    // Rendering too much data goes really slow, so just error
    // beyond a certain threshold.
    const buffer = 5 * 1024 * 1024;

    if (host !== "") {
        args.push("--fake-host=" + host);
    }
    if (mode !== "") {
        args.push("--config-file=fbcode//mode/" + mode.toLowerCase())
    }

    console.log("Start: " + args.join(" "));
    // When run as an Application the root directory will be `/` and there will be nothing on the $PATH.
    // That means `buck2` won't run, and even if we point directly at `buck2`, then `dotslash` will be missing.
    // Therefore we run all shells with `--login` using `exec -- $@` to escape the arguments.
    const { stdout } = await execFile("sh", ["--login", "-c", 'exec -- "$@"', "--", "buck2"].concat(args), {
        maxBuffer: buffer,
        cwd: buckDir,
    });
    console.log("Finish: " + args[0]);
    return stdout;
}

ipcMain.handle('buck2-status', async _ => {
    return await runCommand(["status"], "", "")
});

ipcMain.handle('buck2-targets', async (_, target, host, mode) => {
    validateTarget(target);
    return await runCommand(["targets", target], host, mode);
});

ipcMain.handle('buck2-attributes', async (_, target, host, mode) => {
    validateTarget(target);
    return await runCommand(["uquery", `'${target}'`, "--output-attribute=.*"], host, mode);
});

ipcMain.handle('buck2-providers', async (_, target, host, mode) => {
    validateTarget(target);
    return await runCommand(["audit", "providers", target], host, mode);
});

ipcMain.handle('select-buck-dir', async _ => {
    const result = await dialog.showOpenDialog(mainWindow, {
        properties: ['openDirectory'],
        defaultPath: buckDir,
        message: "Select your Buck2 working directory",
    })
    buckDir = result.filePaths[0];
    return buckDir;
});

ipcMain.handle('current-buck-dir', async _ => {
    return buckDir;
});

app.whenReady().then(() => {
    createWindow()
})
