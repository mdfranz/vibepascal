const outputDiv = document.getElementById('output');
const inputField = document.getElementById('command-input');

let history = [];
let historyIndex = -1;

// Handle Initial Load
window.addEventListener('load', () => {
    sendCommand('LOOK');
    inputField.focus();
});

// Focus input on any click
document.addEventListener('click', () => inputField.focus());

inputField.addEventListener('keydown', async (e) => {
    if (e.key === 'Enter') {
        const cmd = inputField.value.trim();
        if (!cmd) return;

        // Echo command
        appendOutput(`> ${cmd}`, 'command-echo');
        
        // Update history
        history.push(cmd);
        historyIndex = history.length;
        
        inputField.value = '';
        await sendCommand(cmd);
    } else if (e.key === 'ArrowUp') {
        if (historyIndex > 0) {
            historyIndex--;
            inputField.value = history[historyIndex];
        }
    } else if (e.key === 'ArrowDown') {
        if (historyIndex < history.length - 1) {
            historyIndex++;
            inputField.value = history[historyIndex];
        } else {
            historyIndex = history.length;
            inputField.value = '';
        }
    }
});

async function sendCommand(command) {
    try {
        const response = await fetch('/game', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({ command })
        });

        const data = await response.json();
        const cleanText = parseResponse(data.output);
        appendOutput(cleanText, 'game-response');
    } catch (err) {
        appendOutput(`Error: ${err.message}`, 'error');
    }
}

function parseResponse(text) {
    // 1. Identify the core part of the response between LOAD and SAVE
    if (text.includes('> 📂 Game loaded.')) {
        let content = text.split('> 📂 Game loaded.')[1];
        content = content.split('> 💾 Game saved.')[0].trim();
        
        // 2. The Pascal engine prints the room description immediately after LOAD.
        // We want to skip that first description and only show what happened 
        // after the user's command.
        
        // We look for the second occurrence of the room header (📍 ===)
        const lines = content.split('\n');
        let headerCount = 0;
        let finalLines = [];
        
        for (let line of lines) {
            if (line.includes('📍 ===')) {
                headerCount++;
            }
            // Start keeping lines only after we've seen the first automatic room look
            if (headerCount > 1 || !line.includes('📍 ===') && headerCount >= 1) {
                finalLines.push(line);
            }
        }
        
        return finalLines.join('\n').trim();
    }
    return text.trim();
}

function appendOutput(text, className) {
    const div = document.createElement('div');
    div.className = className;
    div.textContent = text;
    outputDiv.appendChild(div);
    
    // Auto-scroll
    outputDiv.scrollTop = outputDiv.scrollHeight;
}
