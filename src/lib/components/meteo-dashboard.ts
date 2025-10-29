import { LitElement, css } from "lit";
import uPlot from "uplot";
import yaml from "js-yaml";

/** Tipos del YAML */
interface SeriesPoint {
  time: string;
  value: number;
}
interface SerieYaml {
  unit?: string;
  values: SeriesPoint[];
}
interface MeteoYaml {
  temperature?: SerieYaml;
  temperatura?: SerieYaml;
  energy?: SerieYaml;
  energia?: SerieYaml;
  power?: SerieYaml;
  potencia?: SerieYaml;
}

/** Bucket por minuto para agregación. */
type Bucket = { t: number[]; e: number[] };

export class MeteoDashboard extends LitElement {
  static styles = css``; // usamos light DOM
  createRenderRoot() { return this; }

  // ===== Refs UI =====
  private statusEl!: HTMLElement;
  private lastTempEl!: HTMLElement;
  private lastEnergyEl!: HTMLElement;
  private chartEl!: HTMLElement;
  private progressEl!: HTMLElement;
  private currentTimeEl!: HTMLElement;
  private nextInEl!: HTMLElement;
  private btnPlay!: HTMLButtonElement;
  private toggleTempBtn!: HTMLButtonElement;
  private toggleEnergyBtn!: HTMLButtonElement;

  // ===== Estado =====
  private timer: number | null = null;
  private secondTimer: number | null = null;
  private nextAt = 0;
  private playing = true;
  private _onThemeChanged?: () => void;

  private data: {
    temperature: { unit: string; values: SeriesPoint[] };
    energy: { unit: string; values: SeriesPoint[] } | null;
  } | null = null;

  private pointer = 0;
  private perMinute = new Map<string, Bucket>();
  private plot: uPlot | null = null;
  private resizeObs: ResizeObserver | null = null;
  private themeObs: MutationObserver | null = null;
  private _onThemeLinkLoad?: () => void;

  private xLabels: string[] = [];
  private showTemp = true;
  private showEnergy = true;

  // ===== Ciclo de vida =====
  connectedCallback(): void {
    super.connectedCallback();
    this.cacheElements();
    this.wireToolbar();
    this.setupChart();
    this.loadFromPublic();

    // Reaccionar a cambios de tema (reconstruir para aplicar ejes/grid)
    this._onThemeChanged = () => this.rebuildChartForTheme();
    window.addEventListener("theme-changed", this._onThemeChanged);

    // Observa cambios en el <link id="theme"> para detectar cambio de tema
    const themeLink = document.querySelector('link#theme') as HTMLLinkElement | null;
    if (themeLink && !this.themeObs) {
      this._onThemeLinkLoad = () => this.rebuildChartForTheme();
      this.themeObs = new MutationObserver(() => {
        // Espera a que cargue la hoja de estilos antes de leer las CSS vars
        if (this._onThemeLinkLoad) {
          themeLink.removeEventListener('load', this._onThemeLinkLoad);
          themeLink.addEventListener('load', this._onThemeLinkLoad, { once: true });
        } else {
          // Fallback por si no existe handler
          setTimeout(() => this.refreshChartColors(), 0);
        }
      });
      this.themeObs.observe(themeLink, { attributes: true, attributeFilter: ['href'] });
    }
  }

  disconnectedCallback(): void {
    super.disconnectedCallback();
    if (this._onThemeChanged)
      window.removeEventListener("theme-changed", this._onThemeChanged);
    if (this.themeObs) { this.themeObs.disconnect(); this.themeObs = null; }
    if (this._onThemeLinkLoad) {
      const themeLink = document.querySelector('link#theme') as HTMLLinkElement | null;
      if (themeLink) themeLink.removeEventListener('load', this._onThemeLinkLoad);
      this._onThemeLinkLoad = undefined;
    }
    if (this.resizeObs) { this.resizeObs.disconnect(); this.resizeObs = null; }
    this.pause();
  }

/** Actualiza colores de series tomando las CSS vars del tema.
 *  Evita redraw si todavía no hay datos para no romper uPlot. */
private refreshChartColors() {
  if (!this.plot) return;

  const css = (v: string, fb: string) =>
    (getComputedStyle(document.documentElement).getPropertyValue(v).trim() || fb);

  const tempColor   = css("--c-temp",    "#7fb3ff");
  const tempFill    = css("--c-temp-a",  "rgba(127,179,255,.30)");
  const energyColor = css("--c-energy",  "#73e0c7");
  const energyFill  = css("--c-energy-a","rgba(115,224,199,.30)");

  // Cambiamos colores sin tocar internals; typings varían → cast puntual a any
  (this.plot as any).setSeries(1, {
    stroke: tempColor,
    points: { show: true, size: 5, stroke: tempColor, fill: tempFill },
  } as any);

  (this.plot as any).setSeries(2, {
    stroke: energyColor,
    points: { show: true, size: 5, stroke: energyColor, fill: energyFill },
  } as any);

  // 🔒 Solo repintamos si ya hay datos (x) para evitar el crash de uPlot
  const hasData = Array.isArray((this.plot as any).data?.[0]) && (this.plot as any).data[0].length > 0;
  if (hasData) this.plot.redraw();
}

// Reconstruye el gráfico para aplicar cambios de ejes/grid por tema
private rebuildChartForTheme() {
  const hadPlot = !!this.plot;
  this.setupChart();
  // Si ya había datos calculados, reponerlos
  if (hadPlot && this.perMinute.size > 0) {
    this.updateChart();
  }
}


  // ===== Elementos y eventos =====
  private cacheElements() {
    this.statusEl = this.querySelector("#status") as HTMLElement;
    this.lastTempEl = this.querySelector("#last-temp") as HTMLElement;
    this.lastEnergyEl = this.querySelector("#last-energy") as HTMLElement;
    this.chartEl = this.querySelector("#chart") as HTMLElement;
    this.progressEl = this.querySelector("#progress") as HTMLElement;
    this.currentTimeEl = this.querySelector("#current-time") as HTMLElement;
    this.nextInEl = this.querySelector("#next-in") as HTMLElement;
    this.btnPlay = this.querySelector("#btnPlay") as HTMLButtonElement;
    this.toggleTempBtn = this.querySelector("#toggleTemp") as HTMLButtonElement;   // <-- fix
    this.toggleEnergyBtn = this.querySelector("#toggleEnergy") as HTMLButtonElement;
  }

  private wireToolbar() {
    this.btnPlay?.addEventListener("click", () => {
      this.playing ? this.pause() : this.startAuto();
      this.btnPlay.setAttribute("aria-pressed", String(this.playing));
    });
    this.toggleTempBtn?.addEventListener("click", () => {
      this.showTemp = !this.showTemp;
      this.toggleTempBtn.setAttribute("aria-pressed", String(this.showTemp));
      this.updateChart();
    });
    this.toggleEnergyBtn?.addEventListener("click", () => {
      this.showEnergy = !this.showEnergy;
      this.toggleEnergyBtn.setAttribute("aria-pressed", String(this.showEnergy));
      this.updateChart();
    });
    // Accesibilidad: barra espaciadora = play/pausa
    this.addEventListener("keydown", (e: KeyboardEvent) => {
      if (e.code === "Space") { e.preventDefault(); this.btnPlay.click(); }
    });
  }

  // ===== Carga YAML =====
  private async loadFromPublic() {
    try {
      this.setStatus("Cargando /data.yml …");
      const res = await fetch("./data.yml", { cache: "no-store" });
      if (!res.ok) throw new Error(`HTTP ${res.status}`);
      const text = await res.text();
      const doc = yaml.load(text) as MeteoYaml;

      const temperature = doc.temperature || doc.temperatura;
      if (!temperature?.values?.length)
        throw new Error("No hay temperature.values en el YAML");
      const energy =
        doc.energy || doc.energia || doc.power || doc.potencia || null;

      const norm = (s: SerieYaml) => ({
        unit: (s.unit || "K").trim(),
        values: s.values
          .map((v) => ({ time: v.time, value: Number(v.value) }))
          .sort((a, b) => this.toSeconds(a.time) - this.toSeconds(b.time)),
      });

      this.data = {
        temperature: norm(temperature),
        energy: energy ? norm(energy) : null,
      };

      // Reset y primer pintado
      this.pointer = 0;
      this.perMinute.clear();
      this.updateChart();

      // Sincroniza con hora real y arranca timers
      this.syncPointerToNow();
      this.startAuto();
    } catch (err: any) {
      console.error(err);
      this.setStatus("Error cargando data.yml: " + err.message);
    }
  }

  // ===== Reproducción =====
  private syncPointerToNow() {
    if (!this.data) return;
    const now = new Date();
    const nowStr = `${String(now.getHours()).padStart(2, "0")}:${String(
      now.getMinutes()
    ).padStart(2, "0")}:${String(now.getSeconds()).padStart(2, "0")}`;
    const nowSec = this.toSeconds(nowStr);

    const arr = this.data.temperature.values;
    let idx = 0;
    for (let i = 0; i < arr.length; i++) {
      const s = this.toSeconds(arr[i].time);
      if (s <= nowSec) idx = i; else break;
    }
    this.pointer = idx;

    this.revealCurrentOnly();
    this.setStatus(`Sincronizado a ${nowStr}. Reproduciendo cada 5s…`);
    this.setNextAt();
  }

  private setNextAt() {
    this.nextAt = Date.now() + 5000;
    if (this.secondTimer) clearInterval(this.secondTimer);
    this.secondTimer = window.setInterval(() => {
      const ms = this.nextAt - Date.now();
      const s = Math.max(0, Math.ceil(ms / 1000));
      this.nextInEl.textContent = String(s);
      if (ms <= 0) clearInterval(this.secondTimer!);
    }, 200);
  }

  private startAuto() {
    if (this.timer) return;
    this.playing = true;
    this.btnPlay?.setAttribute("aria-pressed", "true");
    this.timer = window.setInterval(() => {
      this.revealNext();
      this.setNextAt();
    }, 5000);
  }

  private pause() {
    this.playing = false;
    this.btnPlay?.setAttribute("aria-pressed", "false");
    if (this.timer) { window.clearInterval(this.timer); this.timer = null; }
    if (this.secondTimer) { window.clearInterval(this.secondTimer); this.secondTimer = null; }
  }

  // ===== Chart y datos =====
  private revealCurrentOnly() {
    if (!this.data) return;
    const t = this.data.temperature.values[this.pointer];
    const e = this.data.energy ? this.data.energy.values[this.pointer] : null;

    const tempC = this.toCelsius(t.value, this.data.temperature.unit);
    const energyKWh = e ? this.toKWh(e.value, this.data.energy!.unit) : null;

    this.lastTempEl.textContent = this.fmt2(tempC);
    if (energyKWh !== null) this.lastEnergyEl.textContent = this.fmt2(energyKWh);

    this.currentTimeEl.textContent = t.time;
    this.progressEl.textContent = `${this.pointer + 1} / ${this.data.temperature.values.length}`;

    const minuteKey = t.time.slice(0, 5);
    const bucket = this.perMinute.get(minuteKey) || { t: [], e: [] };
    bucket.t.push(tempC);
    if (energyKWh !== null) bucket.e.push(energyKWh);
    this.perMinute.set(minuteKey, bucket);

    this.updateChart();
  }

  private revealNext() {
    if (!this.data) return;
    if (this.pointer >= this.data.temperature.values.length - 1) {
      this.pause();
      this.setStatus("Fin del dataset");
      return;
    }
    this.pointer++;
    this.revealCurrentOnly();
  }

  // ===== Configurar gráfico =====
  private setupChart() {
    if (this.plot) { this.plot.destroy(); this.plot = null; }

    const getW = () => {
      const w = Math.floor(this.chartEl.getBoundingClientRect().width);
      if (w && w > 0) return w;
      const pw = this.chartEl.parentElement
        ? Math.floor(this.chartEl.parentElement.getBoundingClientRect().width)
        : 0;
      return pw > 0 ? pw : 600;
    };

    const css = (v: string, fallback: string) =>
      getComputedStyle(document.documentElement).getPropertyValue(v).trim() || fallback;

    const tempColor   = css("--c-temp",   "#7fb3ff");
    const tempFill    = css("--c-temp-a", "rgba(127,179,255,.30)");
    const energyColor = css("--c-energy", "#73e0c7");
    const energyFill  = css("--c-energy-a","rgba(115,224,199,.30)");

    const axisColor = css("--muted",  "rgba(190,205,255,.90)");
    const gridColor = css("--border", "rgba(130,150,210,.25)");

    const opts: uPlot.Options = {
      title: "",
      width: getW(),
      height: 360,
      pxAlign: 0.5,
      scales: {
        x: { time: false },
        temp: { auto: true },
        energy: { auto: true },
      },
      axes: [
        {
          stroke: axisColor,
          grid: { show: true, stroke: gridColor },
          values: (_u, vals) =>
            (vals as number[]).map(v => this.xLabels[Math.round(v)] ?? ""),
        },
        { scale: "temp",   label: "°C",  stroke: axisColor },
        { side: 1, scale: "energy", label: "kWh", stroke: axisColor },
      ],
      series: [
        {},
        {
          label: "Temp (°C)",
          scale: "temp",
          points: { show: true, size: 5, stroke: tempColor,   fill: tempFill },
          stroke: tempColor,
          width: 2,
        },
        {
          label: "Energía (kWh)",
          scale: "energy",
          points: { show: true, size: 5, stroke: energyColor, fill: energyFill },
          stroke: energyColor,
          width: 2,
        },
      ],
      cursor: { drag: { x: false, y: false }, focus: { prox: 24 } },
      legend: { show: false },
    };

    this.plot = new uPlot(opts, [[], [], []], this.chartEl);
    this.refreshChartColors(); // aplica colores iniciales

    if (!this.resizeObs) {
      this.resizeObs = new ResizeObserver(() => {
        if (this.plot) this.plot.setSize({ width: getW(), height: 360 });
      });
      this.resizeObs.observe(this.chartEl);
    }
  }

  private updateChart() {
    if (!this.plot) this.setupChart();

    const keys = Array.from(this.perMinute.keys()).sort();
    this.xLabels = keys;
    const x = keys.map((_, i) => i);

    const tSeries = keys.map(k => this.mean(this.perMinute.get(k)!.t));
    const eSeries = keys.map(k => this.mean(this.perMinute.get(k)!.e));

    const tData = this.showTemp   ? tSeries : tSeries.map(() => null);
    const eData = this.showEnergy ? eSeries : eSeries.map(() => null);

    this.plot!.setData([x, tData, eData]);
  }

  // ===== Utilidades =====
  private setStatus(msg: string) { this.statusEl.textContent = msg; }
  private toSeconds(t: string) { const [H,M,S] = t.split(":").map(Number); return H*3600 + M*60 + S; }
  private toCelsius(value: number, unit?: string) {
    const u = (unit || "").toLowerCase();
    if (u === "dk") return value / 10 - 273.15;
    if (u === "k")  return value - 273.15;
    if (u.includes("c")) return value;
    return value - 273.15;
  }
  private toKWh(value: number, unit?: string) {
    const u = (unit || "").toLowerCase();
    if (u === "kwh") return value;
    if (u === "wh")  return value / 1000;
    return value;
  }
  private mean(arr: number[] | undefined): number | null {
    if (!arr || arr.length === 0) return null;
    return arr.reduce((a,b)=>a+b,0)/arr.length;
  }
  private fmt2(n: number | null) {
    if (n === null || Number.isNaN(n)) return "–";
    return (Math.round(n * 100) / 100).toFixed(2);
  }
}

customElements.define("meteo-dashboard", MeteoDashboard);
