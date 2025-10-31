---
oncalls: ['build_infra']
---

# Buck2 Validation Rules

**ALWAYS** run this after changing files in `buck2/app/` or `fbcode/buck2/app/`:

```bash
arc rust-check fbcode//buck2/app/...
```
