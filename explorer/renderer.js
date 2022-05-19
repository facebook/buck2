const $target = document.getElementById("target");
const $completions = document.getElementById("completions");
const $forward = document.getElementById("forward");
const $backward = document.getElementById("backward");
const $buckdir = document.getElementById("buckdir");
const $host = document.getElementById("host");
const $mode = document.getElementById("mode");

// Note that we clear the $output by setting innerHTML,
// which doesn't unregister event handlers.
// Make sure we don't register event handlers on $output children.
const $output = document.getElementById("output");

function setActive(e) {
    const xs = document.getElementsByClassName("active-tab");
    for (let i = 0; i < xs.length; i++) {
        xs[i].className = "";
    }
    e.className = "active-tab";
}

const inCompletions = {}; // {string: true}
const history = [];
let historyIndex = -1;

function addHistory(x) {
    if (!inCompletions[x]) {
        inCompletions[x] = true;
        const e = document.createElement("option");
        e.value = x;
        $completions.prepend(e)
    }
    if (history.length == 0 || history[historyIndex] != x) {
        history.length = historyIndex + 1;
        historyIndex = history.length;
        history.push(x);
        enableForwardBackward();
    }
}

function enableForwardBackward() {
    $backward.disabled = historyIndex <= 0;
    $forward.disabled = historyIndex >= history.length - 1;
}

function moveForwardBackward(delta) {
    historyIndex += delta;
    $target.value = history[historyIndex];
    enableForwardBackward();
    refreshTab();
}

$forward.addEventListener("click", _ => moveForwardBackward(1));
$backward.addEventListener("click", _ => moveForwardBackward(-1));

const targetRegex = /([a-z_]+\/\/[-._/A-Za-z0-9]+\:[-_/+@=.A-Za-z0-9]+)/;

function addTab(id, bold, handler) {
    const elem = document.getElementById(id);
    elem.addEventListener('click', async _ => {
        addHistory($target.value);
        setActive(elem);
        $output.innerHTML = "<progress />";
        let result;
        try {
            result = await handler();
        } catch (err) {
            $output.innerText = err;
            return;
        }

        $output.innerHTML = "";

        if (result.length == 0) {
            const e = document.createElement("i");
            e.innerText = "No output produced";
            $output.appendChild(e);
        } else {
            const xs = result.split(bold);
            for (let i = 0; i < xs.length; i++) {
                if (i % 2 == 0) {
                    const ys = xs[i].split(targetRegex);
                    for (let j = 0; j < ys.length; j++) {
                        if (j % 2 == 0) {
                            $output.appendChild(document.createTextNode(ys[j]))
                        } else {
                            const e = document.createElement("a");
                            e.innerText = ys[j];
                            $output.appendChild(e);
                        }
                    }
                } else {
                    const e = document.createElement("b");
                    e.innerText = xs[i];
                    $output.appendChild(e);
                }
            }
        }
    })
}

function refreshTab() {
    document.getElementsByClassName('active-tab')[0].click();
}

$output.addEventListener('click', (e) => {
    if (e.path.length > 0 && e.path[0].localName == "a") {
        $target.value = e.path[0].innerText;
        refreshTab();
    }
});

$target.addEventListener('keyup', (e) => {
    if (e.key == "Enter") {
        refreshTab();
    }
});

addTab("status", /(\"[a-z_]+\")/, async () => {
    return await window.api.status();
});

addTab("targets", /not-useful/, async () => {
    return await window.api.targets($target.value, $host.value, $mode.value);
});

addTab("attributes", /(\n    \"[^\"]+\":)/, async () => {
    return await window.api.attributes($target.value, $host.value, $mode.value);
});

addTab("providers", /(Providers|[A-Z][A-Za-z]*Info)/, async () => {
    return await window.api.providers($target.value, $host.value, $mode.value);
});

function update_buckdir(dir) {
    $buckdir.title = "Buck2 directory: " + dir;
}

$buckdir.addEventListener('click', async (e) => {
    const res = await window.api.select_buck_dir();
    update_buckdir(res);
});

(async function(){
    const buck_dir = await window.api.current_buck_dir();
    update_buckdir(buck_dir);
})();
