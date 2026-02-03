(() => {
  const state = {
    projects: [],
    agents: [],
    threads: [],
    currentProject: null,
    currentThread: null,
    threadFilter: 'all',
    searchTerm: '',
    agentFilter: '',
  };

  const qs = (sel) => document.querySelector(sel);
  const qsa = (sel) => Array.from(document.querySelectorAll(sel));

  const statusEl = qs('#status-indicator');
  const statusText = statusEl?.querySelector('.status-text');
  const projectList = qs('#project-list');
  const agentList = qs('#agent-list');
  const threadList = qs('#thread-list');
  const threadDetail = qs('#thread-detail');
  const threadSubtitle = qs('#thread-subtitle');
  const threadDetailSubtitle = qs('#thread-detail-subtitle');
  const threadMeta = qs('#thread-meta');
  const refreshProjects = qs('#refresh-projects');
  const refreshAgents = qs('#refresh-agents');
  const refreshThreads = qs('#refresh-threads');
  const agentSelect = qs('#agent-select');
  const threadSearch = qs('#thread-search');
  const filterChips = qsa('.filter-chip');
  const statProjects = qs('#stat-projects .hero-value');
  const statAgents = qs('#stat-agents .hero-value');
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

  const applyThreadFilters = (threads) => {
    let result = threads;
    const term = state.searchTerm.trim().toLowerCase();
    if (term) {
      result = result.filter((thread) => {
        const subject = (thread.last_subject || '').toLowerCase();
        const sender = (thread.last_sender_name || '').toLowerCase();
        const id = (thread.thread_id || '').toLowerCase();
        return subject.includes(term) || sender.includes(term) || id.includes(term);
      });
    }

    if (state.threadFilter === 'unread') {
      result = result.filter((thread) => (thread.unread_count ?? 0) > 0);
    } else if (state.threadFilter === 'ack') {
      result = result.filter((thread) => thread.last_ack_required);
    } else if (state.threadFilter === 'urgent') {
      result = result.filter((thread) => thread.last_importance === 'urgent');
    }

    return result;
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

  const renderAgents = () => {
    if (!agentList) return;
    if (!state.currentProject) {
      setEmpty(agentList, 'Select a project to load agents');
      return;
    }
    if (state.agents.length === 0) {
      setEmpty(agentList, 'No agents registered');
      return;
    }

    agentList.innerHTML = '';

    const allItem = document.createElement('div');
    allItem.className = 'agent-pill';
    if (!state.agentFilter) {
      allItem.classList.add('active');
    }
    allItem.innerHTML = `
      <div class="agent-title">All agents</div>
      <div class="agent-meta">${escapeHtml(String(state.agents.length))} registered</div>
    `;
    allItem.addEventListener('click', () => setAgentFilter(''));
    agentList.appendChild(allItem);

    state.agents.forEach((agent) => {
      const item = document.createElement('div');
      item.className = 'agent-pill';
      if (state.agentFilter === agent.name) {
        item.classList.add('active');
      }
      const model = agent.model || 'unknown model';
      const lastActive = formatTime(agent.last_active_ts);
      item.innerHTML = `
        <div class="agent-title">${escapeHtml(agent.name)}</div>
        <div class="agent-meta">${escapeHtml(model)} • ${escapeHtml(lastActive)}</div>
      `;
      item.addEventListener('click', () => setAgentFilter(agent.name));
      agentList.appendChild(item);
    });
  };

  const populateAgentSelect = () => {
    if (!agentSelect) return;
    agentSelect.innerHTML = '<option value="">All agents</option>';
    state.agents.forEach((agent) => {
      const option = document.createElement('option');
      option.value = agent.name;
      option.textContent = agent.name;
      agentSelect.appendChild(option);
    });
    agentSelect.value = state.agentFilter;
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

    const filtered = applyThreadFilters(state.threads);
    if (filtered.length === 0) {
      setEmpty(threadList, 'No threads match the current filters');
      return;
    }

    threadList.innerHTML = '';
    filtered.forEach((thread) => {
      const item = document.createElement('div');
      item.className = 'list-item';
      if (state.currentThread && thread.thread_id === state.currentThread.thread_id) {
        item.classList.add('active');
      }
      const unread = thread.unread_count ?? 0;
      const importance = thread.last_importance || 'normal';
      const metaParts = [
        thread.last_sender_name || 'Unknown',
        formatTime(thread.last_created_ts),
        `${thread.message_count} messages`,
      ];
      if (importance !== 'normal') {
        metaParts.push(importance);
      }
      const metaHtml = metaParts.map((part) => `<span>${escapeHtml(part)}</span>`).join('');
      const badges = [];
      if (thread.last_ack_required) {
        badges.push('<span class="badge">Ack required</span>');
      }
      if (importance === 'urgent') {
        badges.push('<span class="badge badge-urgent">Urgent</span>');
      }
      if (unread > 0) {
        badges.push(`<span class="badge badge-alert">${escapeHtml(unread)} unread</span>`);
      }
      item.innerHTML = `
        <div class="list-title">${escapeHtml(thread.last_subject || 'Untitled')}</div>
        <div class="list-meta">
          ${metaHtml}
          ${badges.join('')}
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
      const tags = [];
      if (msg.ack_required) {
        tags.push('<span class="badge">Ack required</span>');
      }
      if (msg.importance && msg.importance !== 'normal') {
        const tagClass = msg.importance === 'urgent' ? 'badge badge-urgent' : 'badge';
        tags.push(`<span class="${tagClass}">${escapeHtml(msg.importance)}</span>`);
      }
      card.innerHTML = `
        <div class="message-header">
          <div>
            <div class="message-title">${escapeHtml(subject)}</div>
            <div class="message-meta">${escapeHtml(sender)} • ${escapeHtml(timestamp)}</div>
            <div class="message-tags">${tags.join('')}</div>
          </div>
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
    if (statAgents) statAgents.textContent = `${state.agents.length}`;
    if (statThreads) statThreads.textContent = `${state.threads.length}`;
    if (statUnread) {
      const unread = state.threads.reduce((acc, t) => acc + (t.unread_count || 0), 0);
      statUnread.textContent = `${unread}`;
    }
  };

  const setAgentFilter = async (agentName) => {
    state.agentFilter = agentName;
    if (agentSelect) {
      agentSelect.value = agentName;
    }
    renderAgents();
    await loadThreads(state.currentProject);
  };

  const setThreadFilter = (filter) => {
    state.threadFilter = filter;
    filterChips.forEach((chip) => {
      chip.classList.toggle('active', chip.dataset.filter === filter);
    });
    renderThreads();
  };

  const selectProject = async (project) => {
    state.currentProject = project;
    state.currentThread = null;
    state.agentFilter = '';
    state.searchTerm = '';
    state.threadFilter = 'all';
    if (threadSearch) threadSearch.value = '';
    setThreadFilter('all');
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
    await loadAgents(project);
    await loadThreads(project);
  };

  const selectThread = async (thread) => {
    state.currentThread = thread;
    renderThreads();
    if (threadDetailSubtitle) {
      threadDetailSubtitle.textContent = thread.last_subject || 'Thread detail';
    }
    if (threadMeta) {
      const unread = thread.unread_count ?? 0;
      const ack = thread.last_ack_required ? 'Ack required' : 'No ack';
      threadMeta.textContent = `${thread.message_count} messages · ${ack} · ${unread} unread`;
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

  const loadAgents = async (project) => {
    if (!project) return;
    if (refreshAgents) refreshAgents.disabled = true;
    setStatus('Loading agents', false);
    try {
      const payload = await fetchJson(`/resource/agents/${encodeURIComponent(project.slug)}`);
      state.agents = payload.agents || [];
      populateAgentSelect();
      renderAgents();
      updateStats();
      setStatus('Connected', true);
    } catch (err) {
      console.error(err);
      setStatus('Offline', false);
      setEmpty(agentList, 'Failed to load agents');
    } finally {
      if (refreshAgents) refreshAgents.disabled = false;
    }
  };

  const loadThreads = async (project) => {
    if (!project) return;
    setStatus('Loading threads', false);
    if (refreshThreads) refreshThreads.disabled = true;
    try {
      const params = new URLSearchParams({ limit: '100' });
      if (state.agentFilter) {
        params.set('agent', state.agentFilter);
      }
      const payload = await fetchJson(`/resource/threads/${encodeURIComponent(project.slug)}?${params.toString()}`);
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
  refreshAgents?.addEventListener('click', () => loadAgents(state.currentProject));
  refreshThreads?.addEventListener('click', () => loadThreads(state.currentProject));

  threadSearch?.addEventListener('input', (event) => {
    state.searchTerm = event.target.value || '';
    renderThreads();
  });

  agentSelect?.addEventListener('change', (event) => {
    const value = event.target.value || '';
    setAgentFilter(value);
  });

  filterChips.forEach((chip) => {
    chip.addEventListener('click', () => {
      setThreadFilter(chip.dataset.filter || 'all');
    });
  });

  const handleSseEvent = async (event) => {
    let payload = null;
    try {
      payload = JSON.parse(event.data || '{}');
    } catch (err) {
      console.warn('Failed to parse SSE payload', err);
      return;
    }
    if (!state.currentProject) return;
    const projectSlug = payload.project_slug || payload.project || '';
    const projectKey = payload.project_key || '';
    if (projectSlug && projectSlug !== state.currentProject.slug && projectSlug !== state.currentProject.human_key) {
      return;
    }
    if (projectKey && projectKey !== state.currentProject.slug && projectKey !== state.currentProject.human_key) {
      return;
    }
    await loadThreads(state.currentProject);
    if (state.currentThread && payload.thread_id && state.currentThread.thread_id === payload.thread_id) {
      await loadThreadMessages(state.currentThread);
    }
  };

  const setupSse = () => {
    if (typeof EventSource === 'undefined') return;
    const source = new EventSource('/app/events/mail');
    source.addEventListener('open', () => {
      setStatus('Live updates', true);
    });
    source.addEventListener('error', () => {
      setStatus('Live updates offline', false);
    });
    source.addEventListener('message.sent', handleSseEvent);
    source.addEventListener('message.reply', handleSseEvent);
    source.addEventListener('message.read', handleSseEvent);
    source.addEventListener('message.ack', handleSseEvent);
    source.onmessage = handleSseEvent;
  };

  loadProjects();
  setupSse();
})();
