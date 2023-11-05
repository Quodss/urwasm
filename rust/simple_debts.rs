use contest_algorithms::graph::flow::FlowGraph;
use wasm_bindgen::prelude::*;


#[wasm_bindgen]
pub fn simplify(
    n_people: i32,
    debtor_ids: Vec<i32>,
    creditor_ids: Vec<i32>,
    mut debt_sums: Vec<i32>)
 -> Vec<i32> {
let m = debtor_ids.len();
let mut creditor_ids_opt: Vec<i32> = Vec::new();
let mut debtor_ids_opt: Vec<i32> = Vec::new();
let mut counter: i32 = 0;

for (debtor, creditor) in debtor_ids.iter().zip(&creditor_ids) {
    //  make graph
    let mut graph = FlowGraph::new(n_people as usize, m);
    //  add residual flows
    for k in 0..m {
        graph.add_edge(debtor_ids[k] as usize, creditor_ids[k] as usize, debt_sums[k] as i64, 0, 0);
    }
    //  add optimizing flows
    for k in 0..counter {
        graph.add_edge(debtor_ids_opt[k as usize] as usize, creditor_ids_opt[k as usize] as usize, debt_sums[m + k as usize] as i64, 0, 0);
    }
    //  solve maxflow. flow[::2] has length m+counter
    let (max_flow, flow) = graph.dinic(*debtor as usize, *creditor as usize);
    if max_flow==0 {continue}
    //  put an optimizing transaction in the output
    counter += 1;
    let even_indices_vec: Vec<i64> = flow.iter().step_by(2).cloned().collect();
    println!("max flow between {} and {}: {};", debtor, creditor, max_flow);
    println!("flow between {} and {}: {:?};", debtor, creditor, even_indices_vec);
    creditor_ids_opt.push(*creditor);
    debtor_ids_opt.push(*debtor);
    //  update the rest of the transactions
    //  subtract the flow:
    debt_sums = debt_sums.iter()
                        .zip(flow.iter().step_by(2))
                        .map(|(&a, &b)| a - b as i32)
                        .collect();
    //  push max_flow
    debt_sums.push(max_flow as i32);
    println!("residual debts: {:?}", debt_sums);
}
let mut creditor_ids_out: Vec<i32> = Vec::new();
let mut debtor_ids_out: Vec<i32> = Vec::new();

for k in 0..m {
    debtor_ids_out.push(debtor_ids[k]);
    creditor_ids_out.push(creditor_ids[k]);
}

for k in 0..counter {
    debtor_ids_out.push(debtor_ids_opt[k as usize]);
    creditor_ids_out.push(creditor_ids_opt[k as usize]);
}

assert_eq!(debt_sums.len(), debtor_ids_out.len());
assert_eq!(debt_sums.len(), creditor_ids_out.len());

let mut debtor_ids_out_nonzero: Vec<i32> = Vec::new();
let mut creditor_ids_out_nonzero: Vec<i32> = Vec::new();
let mut debt_sums_out_nonzero: Vec<i32> = Vec::new();

for k in 0..(m + counter as usize) {
    if debt_sums[k] == 0 {continue}
    debtor_ids_out_nonzero.push(debtor_ids_out[k]);
    creditor_ids_out_nonzero.push(creditor_ids_out[k]);
    debt_sums_out_nonzero.push(debt_sums[k]);
}

let result_vec: Vec<i32> = vec![debtor_ids_out_nonzero, creditor_ids_out_nonzero, debt_sums_out_nonzero].into_iter().flatten().collect();
result_vec
}

#[wasm_bindgen]
pub fn process(input: String)
-> String {
    let output_string: String = input.chars().rev().collect();
     output_string
}