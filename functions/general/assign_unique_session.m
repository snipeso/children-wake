function Metadata = assign_unique_session(Metadata)

Metadata.SessionUnique = nan(size(Metadata, 1), 1);
Participants = unique(Metadata.Participant);

UniqueSessionIdx = 1;
for Participant = Participants'
    Sessions = unique(Metadata.Session(strcmp(Metadata.Participant, Participant{1})));
    for Session = Sessions'
        Indexes = strcmp(Metadata.Participant, Participant{1}) & strcmp(Metadata.Session, Session{1});
        Metadata.SessionUnique(Indexes) = UniqueSessionIdx;
        UniqueSessionIdx = UniqueSessionIdx+1;
    end
end

Metadata.SessionUnique = categorical(Metadata.SessionUnique);