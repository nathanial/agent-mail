(() => {
  const state = {
    projects: [],
    threads: [],
    currentProject: null,
    currentThread: null,
  };

  const qs = (sel) => document.querySelector(sel);
  const qsa = (sel) => Array.from(document.querySelectorAll(sel));

  const statusEl = qs('#status-indicator');
  const statusText = statusEl?.querySelector('.status-text');
  const projectList = qs('#project-list');
  const threadList = qs('#thread-list');
  const threadDetail = qs('#thread-detail');
  const threadSubtitle = qs('#thread-subtitle');
  const threadDetailSubtitle = qs('#thread-detail-subtitle');
  const threadMeta = qs('#thread-meta');
  const refreshProjects = qs('#refresh-projects');
  const refreshThreads = qs('#refresh-threads');
  const statProjects = qs('#stat-projects .hero-value');
  const statThreads = qs('#stat-threads .hero-value');
  const statUnread = qs('#stat-unread .hero-value');

  const setStatus = (text, live) => {
    if (!statusEl || !statusText) return;
    statusText.textContent = text;
    statusEl.classList.toggle('is-live', live);
  };

  const setEmpty = (el, message) => {
    if (!el) return;
    el.innerHTML = `<div class="empty">${message}</div>`;
  };

  const escapeHtml = (value) => {
    return String(value ?? '')
      .replaceAll('&', '&amp;')
      .replaceAll('<', '&lt;')
      .replaceAll('>', '&gt;')
      .replaceAll('"', '&quot;')
      .replaceAll("'", '&#39;');
  };

  const formatTime = (seconds) => {
    if (!seconds) return '—';
    const date = new Date(Number(seconds) * 1000);
    if (Number.isNaN(date.getTime())) return '—';
    return date.toLocaleString();
  };

  const fetchJson = async (url) => {
    const resp = await fetch(url, { headers: { 'Accept': 'application/json' } });
    if (!resp.ok) {
      const text = await resp.text();
      throw new Error(`HTTP ${resp.status}: ${text || resp.statusText}`);
    }
    return resp.json();
  };

  const renderProjects = () => {
    if (!projectList) return;
    if (state.projects.length === 0) {
      setEmpty(projectList, 'No projects found');
      return;
    }

    projectList.innerHTML = '';
    state.projects.forEach((project) => {
      const item = document.createElement('div');
      item.className = 'list-item';
      if (state.currentProject && project.slug === state.currentProject.slug) {
        item.classList.add('active');
      }
      const title = project.human_key || project.slug;
      item.innerHTML = `
        <div class="list-title">${escapeHtml(title)}</div>
        <div class="list-meta">
          <span>${escapeHtml(project.slug)}</span>
          <span>${escapeHtml(formatTime(project.created_at))}</span>
        </div>
      `;
      item.addEventListener('click', () => selectProject(project));
      projectList.appendChild(item);
    });
  };

  const renderThreads = () => {
    if (!threadList) return;
    if (!state.currentProject) {
      setEmpty(threadList, 'No project selected');
      return;
    }
    if (state.threads.length === 0) {
      setEmpty(threadList, 'No threads for this project');
      return;
    }

    threadList.innerHTML = '';
    state.threads.forEach((thread) => {
      const item = document.createElement('div');
      item.className = 'list-item';
      if (state.currentThread && thread.thread_id === state.currentThread.thread_id) {
        item.classList.add('active');
      }
      const unread = thread.unread_count ?? 0;
      const importance = thread.last_importance?.toString?.() ?? 'normal';
      const ack = thread.last_ack_required ? 'Ack required' : 'No ack';
      item.innerHTML = `
        <div class="list-title">${escapeHtml(thread.last_subject || 'Untitled')}</div>
        <div class="list-meta">
          <span>${escapeHtml(thread.last_sender_name || 'Unknown')}</span>
          <span>${escapeHtml(formatTime(thread.last_created_ts))}</span>
          <span>${escapeHtml(importance)}</span>
          <span>${escapeHtml(ack)}</span>
          <span>${escapeHtml(unread)} unread</span>
        </div>
      `;
      item.addEventListener('click', () => selectThread(thread));
      threadList.appendChild(item);
    });
  };

  const renderThreadDetail = (messages) => {
    if (!threadDetail) return;
    if (!messages || messages.length === 0) {
      setEmpty(threadDetail, 'No messages for this thread');
      return;
    }

    threadDetail.innerHTML = '';
    messages.forEach((msg) => {
      const card = document.createElement('div');
      card.className = 'message';
      const subject = msg.subject || 'Untitled';
      const sender = msg.sender_name || 'Unknown';
      const timestamp = formatTime(msg.created_ts);
      const body = msg.body_md || '';
      card.innerHTML = `
        <div class="message-header">
          <div class="message-title">${escapeHtml(subject)}</div>
          <div class="message-meta">${escapeHtml(sender)} • ${escapeHtml(timestamp)}</div>
        </div>
        <div class="message-body"></div>
      `;
      const bodyEl = card.querySelector('.message-body');
      if (bodyEl) {
        bodyEl.textContent = body;
      }
      threadDetail.appendChild(card);
    });
  };

  const updateStats = () => {
    if (statProjects) statProjects.textContent = `${state.projects.length}`;
    if (statThreads) statThreads.textContent = `${state.threads.length}`;
    if (statUnread) {
      const unread = state.threads.reduce((acc, t) => acc + (t.unread_count || 0), 0);
      statUnread.textContent = `${unread}`;
    }
  };

  const selectProject = async (project) => {
    state.currentProject = project;
    state.currentThread = null;
    renderProjects();
    if (threadSubtitle) {
      threadSubtitle.textContent = `Project: ${project.human_key || project.slug}`;
    }
    if (threadDetailSubtitle) {
      threadDetailSubtitle.textContent = 'Pick a thread to read messages';
    }
    if (threadMeta) {
      threadMeta.textContent = '—';
    }
    setEmpty(threadDetail, 'No thread selected');
    await loadThreads(project);
  };

  const selectThread = async (thread) => {
    state.currentThread = thread;
    renderThreads();
    if (threadDetailSubtitle) {
      threadDetailSubtitle.textContent = thread.last_subject || 'Thread detail';
    }
    if (threadMeta) {
      threadMeta.textContent = `${thread.message_count} messages`;
    }
    await loadThreadMessages(thread);
  };

  const loadProjects = async () => {
    setStatus('Loading projects', false);
    try {
      const payload = await fetchJson('/resource/projects');
      state.projects = payload.projects || [];
      renderProjects();
      updateStats();
      setStatus('Connected', true);
      if (!state.currentProject && state.projects.length > 0) {
        await selectProject(state.projects[0]);
      }
    } catch (err) {
      console.error(err);
      setStatus('Offline', false);
      setEmpty(projectList, 'Failed to load projects');
    }
  };

  const loadThreads = async (project) => {
    if (!project) return;
    setStatus('Loading threads', false);
    if (refreshThreads) refreshThreads.disabled = true;
    try {
      const payload = await fetchJson(`/resource/threads/${encodeURIComponent(project.slug)}?limit=100`);
      state.threads = payload.threads || [];
      renderThreads();
      updateStats();
      setStatus('Connected', true);
    } catch (err) {
      console.error(err);
      setStatus('Offline', false);
      setEmpty(threadList, 'Failed to load threads');
    } finally {
      if (refreshThreads) refreshThreads.disabled = false;
    }
  };

  const loadThreadMessages = async (thread) => {
    if (!thread || !state.currentProject) return;
    setStatus('Loading thread', false);
    try {
      const url = `/resource/thread/${encodeURIComponent(thread.thread_id)}?project=${encodeURIComponent(state.currentProject.slug)}&include_bodies=true`;
      const payload = await fetchJson(url);
      renderThreadDetail(payload.messages || []);
      setStatus('Connected', true);
    } catch (err) {
      console.error(err);
      setStatus('Offline', false);
      setEmpty(threadDetail, 'Failed to load thread');
    }
  };

  refreshProjects?.addEventListener('click', loadProjects);
  refreshThreads?.addEventListener('click', () => loadThreads(state.currentProject));

  loadProjects();
})();
