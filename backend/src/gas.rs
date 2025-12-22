/// Gas metering and cost model for SwiftSC-Lang contracts
///
/// Gas costs are designed to reflect computational complexity
/// and prevent DoS attacks through resource exhaustion.

#[derive(Debug, Clone, Copy)]
pub struct GasCosts {
    /// Basic arithmetic operations (add, sub, mul, div)
    pub arithmetic: u64,
    /// Memory read/write operations
    pub memory: u64,
    /// Function call overhead
    pub call: u64,
    /// Storage read operation (expensive)
    pub storage_read: u64,
    /// Storage write operation (very expensive)
    pub storage_write: u64,
    /// Event emission
    pub emit_event: u64,
}

impl GasCosts {
    /// Default gas cost model
    pub const fn default() -> Self {
        GasCosts {
            arithmetic: 3,
            memory: 5,
            call: 10,
            storage_read: 100,
            storage_write: 200,
            emit_event: 50,
        }
    }
}

/// Gas metering configuration
pub struct GasConfig {
    /// Gas costs for operations
    pub costs: GasCosts,
    /// Maximum gas per transaction
    pub max_gas: u64,
}

impl Default for GasConfig {
    fn default() -> Self {
        GasConfig {
            costs: GasCosts::default(),
            max_gas: 1_000_000, // 1M gas limit
        }
    }
}

/// Gas meter for tracking consumption
pub struct GasMeter {
    used: u64,
    limit: u64,
}

impl GasMeter {
    pub fn new(limit: u64) -> Self {
        GasMeter { used: 0, limit }
    }

    pub fn consume(&mut self, amount: u64) -> Result<(), &'static str> {
        self.used = self.used.saturating_add(amount);
        if self.used > self.limit {
            Err("Out of gas")
        } else {
            Ok(())
        }
    }

    pub fn remaining(&self) -> u64 {
        self.limit.saturating_sub(self.used)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_gas_meter() {
        let mut meter = GasMeter::new(100);
        assert!(meter.consume(50).is_ok());
        assert_eq!(meter.remaining(), 50);
        assert!(meter.consume(60).is_err()); // Exceeds limit
    }
}
