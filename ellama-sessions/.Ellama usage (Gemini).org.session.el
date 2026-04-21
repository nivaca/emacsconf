#s(ellama-session "Ellama usage (Gemini)" #s(llm-gemini nil nil nil "AIzaSyA4GIY0ywdDG5qsdLT0iv_Nx65LZrD91mQ" "embedding-001" "gemini-3-flash-preview") "/home/nivaca/Dropbox/emacs/ellama-sessions/Ellama usage (Gemini).org" #s(llm-chat-prompt nil nil (#s(llm-chat-prompt-interaction user "How do I use Ellama?" nil) #s(llm-chat-prompt-interaction assistant "**Ellama** is an Emacs package that allows you to interact with Large Language Models (LLMs) directly within your editor. It is primarily designed to work with **Ollama**, but because it uses the `llm` library, it can also connect to OpenAi, Anthropic, and other providers.

Here is a step-by-step guide on how to set up and use Ellama.

---

### 1. Prerequisites
Before setting up the Emacs package, you need the backend that actually runs the AI:
1.  **Install Ollama:** Download it from [ollama.com](https://ollama.com/).
2.  **Download a Model:** Open your terminal and run a model (e.g., Llama 3 or Mistral):
    ```bash
    ollama run llama3
    ```

### 2. Installation
Ellama is available on **MELPA**. You can install it using `M-x package-install RET ellama RET` or via `use-package`.

Add this to your Emacs configuration (`init.el`):

```elisp
(use-package ellama
  :ensure t
  :init
  ;; Setup language
  (setopt ellama-language \"English\")
  ;; Configure the default provider (Ollama)
  (require 'llm-ollama)
  (setopt ellama-provider
          (make-llm-ollama
           :chat-model \"llama3\"    ; Make sure this matches your installed Ollama model
           :embedding-model \"llama3\")))
```

### 3. Basic Configuration
It is highly recommended to set a **prefix key** to make calling commands easier. Most users use `C-c e`:

```elisp
(global-set-key (kbd \"C-c e\") 'ellama-command-map)
```

### 4. How to Use Ellama
Once configured, you can call Ellama functions using your prefix (`C-c e`) followed by a specific command.

#### A. General Interaction
*   **`C-c e c` (ellama-chat):** Opens a new buffer for a continuous chat session.
*   **`C-c e a` (ellama-ask-about):** Ask a question about the currently selected text in your buffer.
*   **`C-c e i` (ellama-ask-interactive):** Ask a standalone question in the minibuffer.

#### B. Text Editing & Transformation
Highlight a block of text and use these:
*   **`C-c e t s` (ellama-summarize):** Summarize the selection.
*   **`C-c e t i` (ellama-improve-wording):** Rewrite the text to sound better.
*   **`C-c e t g` (ellama-improve-grammar):** Fix typos and grammar.
*   **`C-c e t f` (ellama-make-format):** Change the format (e.g., convert a paragraph to a list).

#### C. Coding Helpers
*   **`C-c e w c` (ellama-code-complete):** Provides code completion for the current block.
*   **`C-c e w r` (ellama-code-review):** Reviews the selected code for bugs or optimizations.
*   **`C-c e w a` (ellama-code-add):** Adds a new feature to the selected code based on your prompt.

### 5. Key Features to Know
1.  **Context:** Ellama is \"context-aware.\" If you have a file open or text selected, many commands will automatically include that text as context for the AI.
2.  **Org-Mode Integration:** By default, Ellama chat buffers use Org-mode. This allows you to use `Ctrl-c Ctrl-c` to send messages and enjoy beautiful formatting.
3.  **Providers:** If you want to use something other than Ollama (like GPT-4), you just need to change the `ellama-provider` in your config to use `llm-openai` instead of `llm-ollama`.

### Common Troubleshooting
*   **\"Connection Refused\":** Ensure the Ollama desktop app is running in the background.
*   **Model not found:** Check `ollama list` in your terminal to ensure the model name in your Emacs config matches exactly what is installed.
*   **Slow performance:** This depends on your local hardware (GPU/RAM). If it's too slow, try a smaller model like `phi3` or `tinyllama`." nil)) nil nil nil nil nil nil nil) nil (:dir "/home/nivaca/emacs/mylisp/" :uid "a11f76adb3d0db3bc7c23a8fa25daa05"))