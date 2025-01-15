import './style.css'
import { setupBrain } from './tinkerbrain.ts'

const appDiv = document.querySelector<HTMLDivElement>('#app');

if (appDiv) {
    appDiv.innerHTML = `
        <div id="graph"></div>
        <div class="frame-control">
            <label for="frame-slider">Frame:</label>
            <input type="range" id="frame-slider" min="1" max="20" value="1" step="1">
            <span id="current-frame">1</span>
        </div>
        <div id="transcription-container" style="position: absolute; left: 10px; top: 50%; transform: translateY(-50%); width: 300px;"></div>
    `;
    setupBrain(document.querySelector<HTMLElement>('#graph')!);
}
