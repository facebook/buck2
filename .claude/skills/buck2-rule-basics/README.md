# Buck2 Rule Basics - Interactive Tutorial Skill

## Overview

This skill provides an interactive, step-by-step tutorial for learning Buck2
fundamentals by writing a simple text processing rule. It guides users through 8
progressive steps, teaching core concepts like rules, actions, artifacts,
targets, configurations, and more.

## What Makes This Skill Interactive?

Unlike traditional tutorials that dump all content at once, this skill:

- **Assesses current state** before starting
- **Presents one step at a time** with testing after each
- **Waits for user confirmation** before proceeding
- **Tracks progress** with a visual todo list
- **Adapts to the user's pace** and understanding
- **Encourages experimentation** and questions

## How to Use

### For Users

Simply invoke the skill:

```
use buck2-rule-basics
```

Claude will:

1. Check if you have any existing tutorial files
2. Offer to start fresh or continue from where you left off
3. Guide you through each step interactively
4. Wait for your confirmation before moving to the next step

You can:

- Ask questions at any time
- Request more explanation on any concept
- Experiment with variations
- Take breaks and resume later

### For Claude (Skill Execution)

When this skill is activated, follow these critical rules:

1. **Always start by assessing state**
   - Check for existing `uppercase.bzl`, `BUCK`, `input.txt`
   - Read files to determine current progress
   - Ask user if starting fresh or continuing

2. **Present one step at a time**
   - Introduce the concept
   - Implement the code
   - Test it together
   - Explain what happened
   - **Show file changes**: Summarize what files were created/modified
   - **Remind about editor**: Tell users they can view changes in their editor
   - **STOP** and wait for confirmation

3. **Use AskUserQuestion between steps**
   - "Ready to move to the next step?"
   - "Do you want to explore this more?"
   - "Any questions about this concept?"

4. **Track progress with TodoWrite**
   - Show all 8 steps
   - Mark completed steps
   - Show current step as in_progress
   - Keep upcoming steps visible

5. **Be adaptive**
   - If user is confused → provide more examples
   - If user is advanced → offer to skip explanations
   - If user wants to experiment → encourage and help debug

## Tutorial Structure

The tutorial has 8 steps:

1. **Create Minimal Rule Stub** - Returns empty DefaultInfo()
2. **Add Source File Attribute** - Accept input files
3. **Declare Output Artifact** - Promise to produce output (intentional error)
4. **Create an Action** - Actually produce the output
5. **Understanding Targets** - Unconfigured vs Configured
6. **Add Configuration Support** - Use select() for platform-specific behavior
7. **Add Dependencies** - Make rules compose
8. **Rules vs Macros** - Understand the difference

Each step follows the pattern:

```
Goal → What to do → Code to write → Test → Key concepts → Confirm before next
```

## Files in This Skill

- **SKILL.md** - Main skill definition with step-by-step instructions
- **README.md** - This file, explaining the skill
- **CHANGELOG.md** - History of changes and fixes
- **assets/** - Reference files for the tutorial
  - **rule_template.bzl** - Generic rule template for reference
  - **BUCK.example** - Example BUCK file showing various patterns
  - **test_input.txt** - Sample input for testing
- **references/** - Additional documentation
  - **advanced_patterns.md** - Advanced Buck2 patterns
  - **concepts.md** - Core concepts reference

## Teaching Philosophy

### Show, Don't Tell

- Build and run first
- Then explain what happened
- Concepts are taught in context, not upfront

### Normalize Errors

- Step 3 intentionally causes an error
- Use it as a teaching moment
- "This error is expected and good!"

### Celebrate Wins

- "Great!", "Perfect!", "It worked!"
- Positive reinforcement keeps users engaged

### User-Paced Learning

- Never rush through steps
- Always wait for confirmation
- Encourage questions and exploration

## Common Usage Patterns

### Starting Fresh

```
User: "I want to learn Buck2"
Claude: [Checks for existing files]
Claude: "Starting fresh! Here's the plan..."
Claude: [Creates todo list with 8 steps]
Claude: "Ready for Step 1?"
```

### Resuming

```
User: "Continue the Buck2 tutorial"
Claude: [Reads existing files]
Claude: "You're on Step 5! Let's pick up from there."
Claude: [Resumes with appropriate step]
```

### Exploring

```
User: "I want to try something different"
Claude: "Great! What would you like to experiment with?"
Claude: [Helps user explore variations]
```

## Tips for Teaching

1. **Be conversational** - "Let's try...", "Great job!", "Interesting question!"
2. **Ask questions** - "What do you think will happen?", "Does this make sense?"
3. **Connect concepts** - "Remember in Step 2 when we learned about X? Here's
   where it matters..."
4. **Offer choices** - "Would you like to continue or explore more?"
5. **Provide context** - Explain WHY something works, not just HOW

## Troubleshooting

### If user seems lost

- Go back to previous concept
- Provide simpler examples
- Slow down the pace

### If user is advanced

- Offer to skip basic explanations
- Jump to more interesting topics
- Suggest variations to try

### If errors occur

- Help debug together
- Explain what Buck2 is telling us
- Turn it into a learning opportunity

## Next Steps After Completion

Suggest these resources:

1. Explore Buck2's prelude for real production rules
2. Try more complex rules (multiple outputs, custom providers)
3. Learn BXL for build introspection
4. Read internal docs for advanced patterns
5. Apply learnings to their actual project

## Maintenance

### When updating this skill:

1. Update SKILL.md with the changes
2. Document changes in CHANGELOG.md
3. Test the interactive flow end-to-end
4. Ensure all code examples work
5. Update README.md if teaching approach changes

### Known Issues:

- None currently (see CHANGELOG.md for historical fixes)

## Version History

See [CHANGELOG.md](CHANGELOG.md) for detailed version history.

## Feedback

If you find issues or have suggestions for improving this skill, please provide
feedback through the appropriate channels.
