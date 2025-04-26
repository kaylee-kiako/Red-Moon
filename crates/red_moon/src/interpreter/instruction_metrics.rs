use super::Instruction;
use std::{
    cell::RefCell,
    rc::Rc,
    time::{Duration, Instant},
};

type InstructionMetricsMap =
    crate::FastHashMap<std::mem::Discriminant<Instruction>, (Instruction, InstructionMetrics)>;

#[derive(Clone)]
pub struct InstructionMetrics {
    pub name: &'static str,
    pub count: usize,
    pub min_time: Duration,
    pub max_time: Duration,
    pub total_time: Duration,
}

pub(crate) struct InstructionTracker {
    map: Rc<RefCell<InstructionMetricsMap>>,
    instruction: Instruction,
    start: Instant,
}

impl Drop for InstructionTracker {
    fn drop(&mut self) {
        let duration = Instant::now() - self.start;
        let instruction = self.instruction;

        self.map
            .borrow_mut()
            .entry(std::mem::discriminant(&instruction))
            .and_modify(|(_, metrics)| {
                metrics.count += 1;
                metrics.min_time = metrics.min_time.min(duration);
                metrics.max_time = metrics.max_time.max(duration);
                metrics.total_time += duration;
            })
            .or_insert((
                instruction,
                InstructionMetrics {
                    name: instruction.name(),
                    count: 1,
                    min_time: duration,
                    max_time: duration,
                    total_time: duration,
                },
            ));
    }
}

#[derive(Default)]
pub(crate) struct InstructionMetricTracking {
    map: Rc<RefCell<InstructionMetricsMap>>,
}

impl InstructionMetricTracking {
    pub(crate) fn clear(&mut self) {
        self.map.borrow_mut().clear();
    }

    pub(crate) fn track(&mut self, instruction: Instruction) -> InstructionTracker {
        InstructionTracker {
            map: self.map.clone(),
            instruction,
            start: Instant::now(),
        }
    }

    pub(crate) fn data(&self) -> Vec<InstructionMetrics> {
        let map = self.map.borrow();
        let mut results: Vec<_> = map.values().map(|(_, metrics)| metrics.clone()).collect();

        // sort by count reversed
        results.sort_by_key(|metrics| usize::MAX - metrics.count);
        results
    }
}
