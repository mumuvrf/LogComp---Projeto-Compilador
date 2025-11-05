let b:number = 5; // Variável Global

function soma(x:number, y:number): number {
  let a:number; // Variável Local
  a = x + y; 
  log(a); // Imprime 7
  return a;
}

function main(): void {
  let a:number;
  {
    let b:number;
    a = 3;
    b = soma(a, 4);
    log(b); // Imprime 7
  }
  log(a); // Imprime 3
  log(b); // Imprime 5
}
main();