export function toCelsius(value: number, unit: string | undefined): number {
  const u = (unit || "").toLowerCase();
  if (u === "dk") return value / 10 - 273.15; // deciKelvin
  if (u === "k") return value - 273.15; // Kelvin
  if (u.includes("c")) return value; // °C directo
  return value - 273.15; // fallback: K
}

export function toKWh(value: number, unit: string | undefined): number {
  const u = (unit || "").toLowerCase();
  if (u === "kwh") return value;
  if (u === "wh") return value / 1000;
  return value; // fallback seguro
}

export function mean(values: number[]): number | null {
  if (!values || values.length === 0) return null;
  return values.reduce((a, b) => a + b, 0) / values.length;
}

export function fmt2(n: number | null): string {
  if (n === null || Number.isNaN(n)) return "–";
  return (Math.round(n * 100) / 100).toFixed(2);
}
