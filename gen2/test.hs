data ListClosedWorkflowExecutionsInput = ListClosedWorkflowExecutionsInput
    { domain :: DomainName
    , startTimeFilter :: ExecutionTimeFilter
    , closeTimeFilter :: ExecutionTimeFilter
    , executionFilter :: WorkflowExecutionFilter
    , closeStatusFilter :: CloseStatusFilter
    , typeFilter :: WorkflowTypeFilter
    , tagFilter :: TagFilter
    , nextPageToken :: PageToken
    , maximumPageSize :: PageSize
    , reverseOrder :: ReverseOrder
    } deriving (Eq,Show)
newtype ActivityTaskStatus = ActivityTaskStatus
    { cancelRequested :: Canceled
    } deriving (Eq,Show)
newtype DomainAlreadyExistsFault = DomainAlreadyExistsFault
    { message :: ErrorMessage
    } deriving (Eq,Show)
data WorkflowExecutionCancelRequestedEventAttributes = WorkflowExecutionCancelRequestedEventAttributes
    { externalWorkflowExecution :: WorkflowExecution
    , externalInitiatedEventId :: EventId
    , cause :: WorkflowExecutionCancelRequestedCause
    } deriving (Eq,Show)
newtype LimitExceededFault = LimitExceededFault
    { message :: ErrorMessage
    } deriving (Eq,Show)
newtype WorkflowExecutionAlreadyStartedFault = WorkflowExecutionAlreadyStartedFault
    { message :: ErrorMessage
    } deriving (Eq,Show)
data CountPendingDecisionTasksInput = CountPendingDecisionTasksInput
    { domain :: DomainName
    , taskList :: TaskList
    } deriving (Eq,Show)
data RequestCancelExternalWorkflowExecutionDecisionAttributes = RequestCancelExternalWorkflowExecutionDecisionAttributes
    { workflowId :: WorkflowId
    , runId :: RunIdOptional
    , control :: Data
    } deriving (Eq,Show)
data DecisionTaskScheduledEventAttributes = DecisionTaskScheduledEventAttributes
    { taskList :: TaskList
    , taskPriority :: TaskPriority
    , startToCloseTimeout :: DurationInSecondsOptional
    } deriving (Eq,Show)
data RegisterDomainInput = RegisterDomainInput
    { name :: DomainName
    , description :: Description
    , workflowExecutionRetentionPeriodInDays :: DurationInDays
    } deriving (Eq,Show)
data WorkflowExecutionCompletedEventAttributes = WorkflowExecutionCompletedEventAttributes
    { result :: Data
    , decisionTaskCompletedEventId :: EventId
    } deriving (Eq,Show)
data ExecutionTimeFilter = ExecutionTimeFilter
    { oldestDate :: Timestamp
    , latestDate :: Timestamp
    } deriving (Eq,Show)
data StartTimerFailedEventAttributes = StartTimerFailedEventAttributes
    { timerId :: TimerId
    , cause :: StartTimerFailedCause
    , decisionTaskCompletedEventId :: EventId
    } deriving (Eq,Show)
data RequestCancelExternalWorkflowExecutionInitiatedEventAttributes = RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
    { workflowId :: WorkflowId
    , runId :: RunIdOptional
    , decisionTaskCompletedEventId :: EventId
    , control :: Data
    } deriving (Eq,Show)
data RespondDecisionTaskCompletedInput = RespondDecisionTaskCompletedInput
    { taskToken :: TaskToken
    , decisions :: DecisionList
    , executionContext :: Data
    } deriving (Eq,Show)
data RecordMarkerFailedEventAttributes = RecordMarkerFailedEventAttributes
    { markerName :: MarkerName
    , cause :: RecordMarkerFailedCause
    , decisionTaskCompletedEventId :: EventId
    } deriving (Eq,Show)
data WorkflowExecutionCount = WorkflowExecutionCount
    { count :: Count
    , truncated :: Truncated
    } deriving (Eq,Show)
data ActivityTaskScheduledEventAttributes = ActivityTaskScheduledEventAttributes
    { activityType :: ActivityType
    , activityId :: ActivityId
    , input :: Data
    , control :: Data
    , scheduleToStartTimeout :: DurationInSecondsOptional
    , scheduleToCloseTimeout :: DurationInSecondsOptional
    , startToCloseTimeout :: DurationInSecondsOptional
    , taskList :: TaskList
    , taskPriority :: TaskPriority
    , decisionTaskCompletedEventId :: EventId
    , heartbeatTimeout :: DurationInSecondsOptional
    } deriving (Eq,Show)
newtype CloseStatusFilter = CloseStatusFilter
    { status :: CloseStatus
    } deriving (Eq,Show)
data RespondActivityTaskCompletedInput = RespondActivityTaskCompletedInput
    { taskToken :: TaskToken
    , result :: Data
    } deriving (Eq,Show)
data PollForActivityTaskInput = PollForActivityTaskInput
    { domain :: DomainName
    , taskList :: TaskList
    , identity :: Identity
    } deriving (Eq,Show)
data History = History
    { events :: HistoryEventList
    , nextPageToken :: PageToken
    } deriving (Eq,Show)
data DescribeWorkflowExecutionInput = DescribeWorkflowExecutionInput
    { domain :: DomainName
    , execution :: WorkflowExecution
    } deriving (Eq,Show)
data ScheduleActivityTaskDecisionAttributes = ScheduleActivityTaskDecisionAttributes
    { activityType :: ActivityType
    , activityId :: ActivityId
    , control :: Data
    , input :: Data
    , scheduleToCloseTimeout :: DurationInSecondsOptional
    , taskList :: TaskList
    , taskPriority :: TaskPriority
    , scheduleToStartTimeout :: DurationInSecondsOptional
    , startToCloseTimeout :: DurationInSecondsOptional
    , heartbeatTimeout :: DurationInSecondsOptional
    } deriving (Eq,Show)
data ActivityTypeConfiguration = ActivityTypeConfiguration
    { defaultTaskStartToCloseTimeout :: DurationInSecondsOptional
    , defaultTaskHeartbeatTimeout :: DurationInSecondsOptional
    , defaultTaskList :: TaskList
    , defaultTaskPriority :: TaskPriority
    , defaultTaskScheduleToStartTimeout :: DurationInSecondsOptional
    , defaultTaskScheduleToCloseTimeout :: DurationInSecondsOptional
    } deriving (Eq,Show)
data DecisionTask = DecisionTask
    { taskToken :: TaskToken
    , startedEventId :: EventId
    , workflowExecution :: WorkflowExecution
    , workflowType :: WorkflowType
    , events :: HistoryEventList
    , nextPageToken :: PageToken
    , previousStartedEventId :: EventId
    } deriving (Eq,Show)
data ActivityType = ActivityType
    { name :: Name
    , version :: Version
    } deriving (Eq,Show)
data WorkflowTypeInfo = WorkflowTypeInfo
    { workflowType :: WorkflowType
    , status :: RegistrationStatus
    , description :: Description
    , creationDate :: Timestamp
    , deprecationDate :: Timestamp
    } deriving (Eq,Show)
data ChildWorkflowExecutionCompletedEventAttributes = ChildWorkflowExecutionCompletedEventAttributes
    { workflowExecution :: WorkflowExecution
    , workflowType :: WorkflowType
    , result :: Data
    , initiatedEventId :: EventId
    , startedEventId :: EventId
    } deriving (Eq,Show)
data WorkflowExecutionOpenCounts = WorkflowExecutionOpenCounts
    { openActivityTasks :: Count
    , openDecisionTasks :: OpenDecisionTasksCount
    , openTimers :: Count
    , openChildWorkflowExecutions :: Count
    } deriving (Eq,Show)
data ScheduleActivityTaskFailedEventAttributes = ScheduleActivityTaskFailedEventAttributes
    { activityType :: ActivityType
    , activityId :: ActivityId
    , cause :: ScheduleActivityTaskFailedCause
    , decisionTaskCompletedEventId :: EventId
    } deriving (Eq,Show)
data MarkerRecordedEventAttributes = MarkerRecordedEventAttributes
    { markerName :: MarkerName
    , details :: Data
    , decisionTaskCompletedEventId :: EventId
    } deriving (Eq,Show)
data SignalExternalWorkflowExecutionDecisionAttributes = SignalExternalWorkflowExecutionDecisionAttributes
    { workflowId :: WorkflowId
    , runId :: RunIdOptional
    , signalName :: SignalName
    , input :: Data
    , control :: Data
    } deriving (Eq,Show)
newtype OperationNotPermittedFault = OperationNotPermittedFault
    { message :: ErrorMessage
    } deriving (Eq,Show)
data RecordMarkerDecisionAttributes = RecordMarkerDecisionAttributes
    { markerName :: MarkerName
    , details :: Data
    } deriving (Eq,Show)
data WorkflowExecutionDetail = WorkflowExecutionDetail
    { executionInfo :: WorkflowExecutionInfo
    , executionConfiguration :: WorkflowExecutionConfiguration
    , openCounts :: WorkflowExecutionOpenCounts
    , latestActivityTaskTimestamp :: Timestamp
    , latestExecutionContext :: Data
    } deriving (Eq,Show)
data CompleteWorkflowExecutionFailedEventAttributes = CompleteWorkflowExecutionFailedEventAttributes
    { cause :: CompleteWorkflowExecutionFailedCause
    , decisionTaskCompletedEventId :: EventId
    } deriving (Eq,Show)
data DeprecateWorkflowTypeInput = DeprecateWorkflowTypeInput
    { domain :: DomainName
    , workflowType :: WorkflowType
    } deriving (Eq,Show)
data StartTimerDecisionAttributes = StartTimerDecisionAttributes
    { timerId :: TimerId
    , control :: Data
    , startToFireTimeout :: DurationInSeconds
    } deriving (Eq,Show)
data ActivityTypeInfos = ActivityTypeInfos
    { typeInfos :: ActivityTypeInfoList
    , nextPageToken :: PageToken
    } deriving (Eq,Show)
data DescribeWorkflowTypeInput = DescribeWorkflowTypeInput
    { domain :: DomainName
    , workflowType :: WorkflowType
    } deriving (Eq,Show)
data ActivityTask = ActivityTask
    { taskToken :: TaskToken
    , activityId :: ActivityId
    , startedEventId :: EventId
    , workflowExecution :: WorkflowExecution
    , activityType :: ActivityType
    , input :: Data
    } deriving (Eq,Show)
data RequestCancelWorkflowExecutionInput = RequestCancelWorkflowExecutionInput
    { domain :: DomainName
    , workflowId :: WorkflowId
    , runId :: RunIdOptional
    } deriving (Eq,Show)
newtype UnknownResourceFault = UnknownResourceFault
    { message :: ErrorMessage
    } deriving (Eq,Show)
newtype DefaultUndefinedFault = DefaultUndefinedFault
    { message :: ErrorMessage
    } deriving (Eq,Show)
newtype TypeDeprecatedFault = TypeDeprecatedFault
    { message :: ErrorMessage
    } deriving (Eq,Show)
data RequestCancelExternalWorkflowExecutionFailedEventAttributes = RequestCancelExternalWorkflowExecutionFailedEventAttributes
    { workflowId :: WorkflowId
    , runId :: RunIdOptional
    , cause :: RequestCancelExternalWorkflowExecutionFailedCause
    , initiatedEventId :: EventId
    , decisionTaskCompletedEventId :: EventId
    , control :: Data
    } deriving (Eq,Show)
data ActivityTypeInfo = ActivityTypeInfo
    { activityType :: ActivityType
    , status :: RegistrationStatus
    , description :: Description
    , creationDate :: Timestamp
    , deprecationDate :: Timestamp
    } deriving (Eq,Show)
data TimerCanceledEventAttributes = TimerCanceledEventAttributes
    { timerId :: TimerId
    , startedEventId :: EventId
    , decisionTaskCompletedEventId :: EventId
    } deriving (Eq,Show)
data WorkflowExecutionStartedEventAttributes = WorkflowExecutionStartedEventAttributes
    { input :: Data
    , executionStartToCloseTimeout :: DurationInSecondsOptional
    , taskStartToCloseTimeout :: DurationInSecondsOptional
    , childPolicy :: ChildPolicy
    , taskList :: TaskList
    , workflowType :: WorkflowType
    , tagList :: TagList
    , taskPriority :: TaskPriority
    , continuedExecutionRunId :: RunIdOptional
    , parentWorkflowExecution :: WorkflowExecution
    , parentInitiatedEventId :: EventId
    } deriving (Eq,Show)
data WorkflowTypeConfiguration = WorkflowTypeConfiguration
    { defaultTaskStartToCloseTimeout :: DurationInSecondsOptional
    , defaultExecutionStartToCloseTimeout :: DurationInSecondsOptional
    , defaultTaskList :: TaskList
    , defaultTaskPriority :: TaskPriority
    , defaultChildPolicy :: ChildPolicy
    } deriving (Eq,Show)
data CountOpenWorkflowExecutionsInput = CountOpenWorkflowExecutionsInput
    { domain :: DomainName
    , startTimeFilter :: ExecutionTimeFilter
    , typeFilter :: WorkflowTypeFilter
    , tagFilter :: TagFilter
    , executionFilter :: WorkflowExecutionFilter
    } deriving (Eq,Show)
data WorkflowType = WorkflowType
    { name :: Name
    , version :: Version
    } deriving (Eq,Show)
data ActivityTaskCompletedEventAttributes = ActivityTaskCompletedEventAttributes
    { result :: Data
    , scheduledEventId :: EventId
    , startedEventId :: EventId
    } deriving (Eq,Show)
data DecisionTaskTimedOutEventAttributes = DecisionTaskTimedOutEventAttributes
    { timeoutType :: DecisionTaskTimeoutType
    , scheduledEventId :: EventId
    , startedEventId :: EventId
    } deriving (Eq,Show)
data CountPendingActivityTasksInput = CountPendingActivityTasksInput
    { domain :: DomainName
    , taskList :: TaskList
    } deriving (Eq,Show)
data ChildWorkflowExecutionStartedEventAttributes = ChildWorkflowExecutionStartedEventAttributes
    { workflowExecution :: WorkflowExecution
    , workflowType :: WorkflowType
    , initiatedEventId :: EventId
    } deriving (Eq,Show)
data CancelTimerFailedEventAttributes = CancelTimerFailedEventAttributes
    { timerId :: TimerId
    , cause :: CancelTimerFailedCause
    , decisionTaskCompletedEventId :: EventId
    } deriving (Eq,Show)
newtype Run = Run
    { runId :: RunId
    } deriving (Eq,Show)
newtype WorkflowExecutionFilter = WorkflowExecutionFilter
    { workflowId :: WorkflowId
    } deriving (Eq,Show)
data ActivityTaskCanceledEventAttributes = ActivityTaskCanceledEventAttributes
    { details :: Data
    , scheduledEventId :: EventId
    , startedEventId :: EventId
    , latestCancelRequestedEventId :: EventId
    } deriving (Eq,Show)
data WorkflowExecutionInfos = WorkflowExecutionInfos
    { executionInfos :: WorkflowExecutionInfoList
    , nextPageToken :: PageToken
    } deriving (Eq,Show)
data StartChildWorkflowExecutionDecisionAttributes = StartChildWorkflowExecutionDecisionAttributes
    { workflowType :: WorkflowType
    , workflowId :: WorkflowId
    , control :: Data
    , input :: Data
    , executionStartToCloseTimeout :: DurationInSecondsOptional
    , taskList :: TaskList
    , taskPriority :: TaskPriority
    , taskStartToCloseTimeout :: DurationInSecondsOptional
    , childPolicy :: ChildPolicy
    , tagList :: TagList
    } deriving (Eq,Show)
data ListWorkflowTypesInput = ListWorkflowTypesInput
    { domain :: DomainName
    , name :: Name
    , registrationStatus :: RegistrationStatus
    , nextPageToken :: PageToken
    , maximumPageSize :: PageSize
    , reverseOrder :: ReverseOrder
    } deriving (Eq,Show)
data FailWorkflowExecutionDecisionAttributes = FailWorkflowExecutionDecisionAttributes
    { reason :: FailureReason
    , details :: Data
    } deriving (Eq,Show)
data RespondActivityTaskFailedInput = RespondActivityTaskFailedInput
    { taskToken :: TaskToken
    , reason :: FailureReason
    , details :: Data
    } deriving (Eq,Show)
data ActivityTypeDetail = ActivityTypeDetail
    { typeInfo :: ActivityTypeInfo
    , configuration :: ActivityTypeConfiguration
    } deriving (Eq,Show)
data RegisterWorkflowTypeInput = RegisterWorkflowTypeInput
    { domain :: DomainName
    , name :: Name
    , version :: Version
    , description :: Description
    , defaultTaskStartToCloseTimeout :: DurationInSecondsOptional
    , defaultExecutionStartToCloseTimeout :: DurationInSecondsOptional
    , defaultTaskList :: TaskList
    , defaultTaskPriority :: TaskPriority
    , defaultChildPolicy :: ChildPolicy
    } deriving (Eq,Show)
newtype TypeAlreadyExistsFault = TypeAlreadyExistsFault
    { message :: ErrorMessage
    } deriving (Eq,Show)
data ActivityTaskTimedOutEventAttributes = ActivityTaskTimedOutEventAttributes
    { timeoutType :: ActivityTaskTimeoutType
    , scheduledEventId :: EventId
    , startedEventId :: EventId
    , details :: LimitedData
    } deriving (Eq,Show)
data RequestCancelActivityTaskFailedEventAttributes = RequestCancelActivityTaskFailedEventAttributes
    { activityId :: ActivityId
    , cause :: RequestCancelActivityTaskFailedCause
    , decisionTaskCompletedEventId :: EventId
    } deriving (Eq,Show)
newtype CompleteWorkflowExecutionDecisionAttributes = CompleteWorkflowExecutionDecisionAttributes
    { result :: Data
    } deriving (Eq,Show)
data DecisionTaskStartedEventAttributes = DecisionTaskStartedEventAttributes
    { identity :: Identity
    , scheduledEventId :: EventId
    } deriving (Eq,Show)
data PollForDecisionTaskInput = PollForDecisionTaskInput
    { domain :: DomainName
    , taskList :: TaskList
    , identity :: Identity
    , nextPageToken :: PageToken
    , maximumPageSize :: PageSize
    , reverseOrder :: ReverseOrder
    } deriving (Eq,Show)
data ChildWorkflowExecutionTimedOutEventAttributes = ChildWorkflowExecutionTimedOutEventAttributes
    { workflowExecution :: WorkflowExecution
    , workflowType :: WorkflowType
    , timeoutType :: WorkflowExecutionTimeoutType
    , initiatedEventId :: EventId
    , startedEventId :: EventId
    } deriving (Eq,Show)
data StartChildWorkflowExecutionInitiatedEventAttributes = StartChildWorkflowExecutionInitiatedEventAttributes
    { workflowId :: WorkflowId
    , workflowType :: WorkflowType
    , control :: Data
    , input :: Data
    , executionStartToCloseTimeout :: DurationInSecondsOptional
    , taskList :: TaskList
    , taskPriority :: TaskPriority
    , decisionTaskCompletedEventId :: EventId
    , childPolicy :: ChildPolicy
    , taskStartToCloseTimeout :: DurationInSecondsOptional
    , tagList :: TagList
    } deriving (Eq,Show)
data CancelWorkflowExecutionFailedEventAttributes = CancelWorkflowExecutionFailedEventAttributes
    { cause :: CancelWorkflowExecutionFailedCause
    , decisionTaskCompletedEventId :: EventId
    } deriving (Eq,Show)
data WorkflowExecutionTerminatedEventAttributes = WorkflowExecutionTerminatedEventAttributes
    { reason :: TerminateReason
    , details :: Data
    , childPolicy :: ChildPolicy
    , cause :: WorkflowExecutionTerminatedCause
    } deriving (Eq,Show)
data StartWorkflowExecutionInput = StartWorkflowExecutionInput
    { domain :: DomainName
    , workflowId :: WorkflowId
    , workflowType :: WorkflowType
    , taskList :: TaskList
    , taskPriority :: TaskPriority
    , input :: Data
    , executionStartToCloseTimeout :: DurationInSecondsOptional
    , tagList :: TagList
    , taskStartToCloseTimeout :: DurationInSecondsOptional
    , childPolicy :: ChildPolicy
    } deriving (Eq,Show)
newtype TaskList = TaskList
    { name :: Name
    } deriving (Eq,Show)
data ListDomainsInput = ListDomainsInput
    { nextPageToken :: PageToken
    , registrationStatus :: RegistrationStatus
    , maximumPageSize :: PageSize
    , reverseOrder :: ReverseOrder
    } deriving (Eq,Show)
data ChildWorkflowExecutionCanceledEventAttributes = ChildWorkflowExecutionCanceledEventAttributes
    { workflowExecution :: WorkflowExecution
    , workflowType :: WorkflowType
    , details :: Data
    , initiatedEventId :: EventId
    , startedEventId :: EventId
    } deriving (Eq,Show)
data WorkflowExecutionInfo = WorkflowExecutionInfo
    { execution :: WorkflowExecution
    , workflowType :: WorkflowType
    , startTimestamp :: Timestamp
    , closeTimestamp :: Timestamp
    , executionStatus :: ExecutionStatus
    , closeStatus :: CloseStatus
    , parent :: WorkflowExecution
    , tagList :: TagList
    , cancelRequested :: Canceled
    } deriving (Eq,Show)
data ListOpenWorkflowExecutionsInput = ListOpenWorkflowExecutionsInput
    { domain :: DomainName
    , startTimeFilter :: ExecutionTimeFilter
    , typeFilter :: WorkflowTypeFilter
    , tagFilter :: TagFilter
    , nextPageToken :: PageToken
    , maximumPageSize :: PageSize
    , reverseOrder :: ReverseOrder
    , executionFilter :: WorkflowExecutionFilter
    } deriving (Eq,Show)
data SignalExternalWorkflowExecutionFailedEventAttributes = SignalExternalWorkflowExecutionFailedEventAttributes
    { workflowId :: WorkflowId
    , runId :: RunIdOptional
    , cause :: SignalExternalWorkflowExecutionFailedCause
    , initiatedEventId :: EventId
    , decisionTaskCompletedEventId :: EventId
    , control :: Data
    } deriving (Eq,Show)
newtype TagFilter = TagFilter
    { tag :: Tag
    } deriving (Eq,Show)
data ActivityTaskStartedEventAttributes = ActivityTaskStartedEventAttributes
    { identity :: Identity
    , scheduledEventId :: EventId
    } deriving (Eq,Show)
data ListActivityTypesInput = ListActivityTypesInput
    { domain :: DomainName
    , name :: Name
    , registrationStatus :: RegistrationStatus
    , nextPageToken :: PageToken
    , maximumPageSize :: PageSize
    , reverseOrder :: ReverseOrder
    } deriving (Eq,Show)
data RegisterActivityTypeInput = RegisterActivityTypeInput
    { domain :: DomainName
    , name :: Name
    , version :: Version
    , description :: Description
    , defaultTaskStartToCloseTimeout :: DurationInSecondsOptional
    , defaultTaskHeartbeatTimeout :: DurationInSecondsOptional
    , defaultTaskList :: TaskList
    , defaultTaskPriority :: TaskPriority
    , defaultTaskScheduleToStartTimeout :: DurationInSecondsOptional
    , defaultTaskScheduleToCloseTimeout :: DurationInSecondsOptional
    } deriving (Eq,Show)
data WorkflowTypeDetail = WorkflowTypeDetail
    { typeInfo :: WorkflowTypeInfo
    , configuration :: WorkflowTypeConfiguration
    } deriving (Eq,Show)
data ActivityTaskCancelRequestedEventAttributes = ActivityTaskCancelRequestedEventAttributes
    { decisionTaskCompletedEventId :: EventId
    , activityId :: ActivityId
    } deriving (Eq,Show)
data WorkflowExecutionTimedOutEventAttributes = WorkflowExecutionTimedOutEventAttributes
    { timeoutType :: WorkflowExecutionTimeoutType
    , childPolicy :: ChildPolicy
    } deriving (Eq,Show)
data ChildWorkflowExecutionTerminatedEventAttributes = ChildWorkflowExecutionTerminatedEventAttributes
    { workflowExecution :: WorkflowExecution
    , workflowType :: WorkflowType
    , initiatedEventId :: EventId
    , startedEventId :: EventId
    } deriving (Eq,Show)
data WorkflowExecutionCanceledEventAttributes = WorkflowExecutionCanceledEventAttributes
    { details :: Data
    , decisionTaskCompletedEventId :: EventId
    } deriving (Eq,Show)
data CountClosedWorkflowExecutionsInput = CountClosedWorkflowExecutionsInput
    { domain :: DomainName
    , startTimeFilter :: ExecutionTimeFilter
    , closeTimeFilter :: ExecutionTimeFilter
    , executionFilter :: WorkflowExecutionFilter
    , typeFilter :: WorkflowTypeFilter
    , tagFilter :: TagFilter
    , closeStatusFilter :: CloseStatusFilter
    } deriving (Eq,Show)
data WorkflowExecutionSignaledEventAttributes = WorkflowExecutionSignaledEventAttributes
    { signalName :: SignalName
    , input :: Data
    , externalWorkflowExecution :: WorkflowExecution
    , externalInitiatedEventId :: EventId
    } deriving (Eq,Show)
data DomainInfos = DomainInfos
    { domainInfos :: DomainInfoList
    , nextPageToken :: PageToken
    } deriving (Eq,Show)
data TimerStartedEventAttributes = TimerStartedEventAttributes
    { timerId :: TimerId
    , control :: Data
    , startToFireTimeout :: DurationInSeconds
    , decisionTaskCompletedEventId :: EventId
    } deriving (Eq,Show)
newtype RequestCancelActivityTaskDecisionAttributes = RequestCancelActivityTaskDecisionAttributes
    { activityId :: ActivityId
    } deriving (Eq,Show)
data Decision = Decision
    { decisionType :: DecisionType
    , scheduleActivityTaskDecisionAttributes :: ScheduleActivityTaskDecisionAttributes
    , requestCancelActivityTaskDecisionAttributes :: RequestCancelActivityTaskDecisionAttributes
    , completeWorkflowExecutionDecisionAttributes :: CompleteWorkflowExecutionDecisionAttributes
    , failWorkflowExecutionDecisionAttributes :: FailWorkflowExecutionDecisionAttributes
    , cancelWorkflowExecutionDecisionAttributes :: CancelWorkflowExecutionDecisionAttributes
    , continueAsNewWorkflowExecutionDecisionAttributes :: ContinueAsNewWorkflowExecutionDecisionAttributes
    , recordMarkerDecisionAttributes :: RecordMarkerDecisionAttributes
    , startTimerDecisionAttributes :: StartTimerDecisionAttributes
    , cancelTimerDecisionAttributes :: CancelTimerDecisionAttributes
    , signalExternalWorkflowExecutionDecisionAttributes :: SignalExternalWorkflowExecutionDecisionAttributes
    , requestCancelExternalWorkflowExecutionDecisionAttributes :: RequestCancelExternalWorkflowExecutionDecisionAttributes
    , startChildWorkflowExecutionDecisionAttributes :: StartChildWorkflowExecutionDecisionAttributes
    } deriving (Eq,Show)
data TimerFiredEventAttributes = TimerFiredEventAttributes
    { timerId :: TimerId
    , startedEventId :: EventId
    } deriving (Eq,Show)
data RespondActivityTaskCanceledInput = RespondActivityTaskCanceledInput
    { taskToken :: TaskToken
    , details :: Data
    } deriving (Eq,Show)
newtype DomainConfiguration = DomainConfiguration
    { workflowExecutionRetentionPeriodInDays :: DurationInDays
    } deriving (Eq,Show)
data ExternalWorkflowExecutionSignaledEventAttributes = ExternalWorkflowExecutionSignaledEventAttributes
    { workflowExecution :: WorkflowExecution
    , initiatedEventId :: EventId
    } deriving (Eq,Show)
newtype CancelWorkflowExecutionDecisionAttributes = CancelWorkflowExecutionDecisionAttributes
    { details :: Data
    } deriving (Eq,Show)
data ActivityTaskFailedEventAttributes = ActivityTaskFailedEventAttributes
    { reason :: FailureReason
    , details :: Data
    , scheduledEventId :: EventId
    , startedEventId :: EventId
    } deriving (Eq,Show)
data FailWorkflowExecutionFailedEventAttributes = FailWorkflowExecutionFailedEventAttributes
    { cause :: FailWorkflowExecutionFailedCause
    , decisionTaskCompletedEventId :: EventId
    } deriving (Eq,Show)
newtype DeprecateDomainInput = DeprecateDomainInput
    { name :: DomainName
    } deriving (Eq,Show)
data StartChildWorkflowExecutionFailedEventAttributes = StartChildWorkflowExecutionFailedEventAttributes
    { workflowType :: WorkflowType
    , cause :: StartChildWorkflowExecutionFailedCause
    , workflowId :: WorkflowId
    , initiatedEventId :: EventId
    , decisionTaskCompletedEventId :: EventId
    , control :: Data
    } deriving (Eq,Show)
newtype DomainDeprecatedFault = DomainDeprecatedFault
    { message :: ErrorMessage
    } deriving (Eq,Show)
data WorkflowTypeFilter = WorkflowTypeFilter
    { name :: Name
    , version :: VersionOptional
    } deriving (Eq,Show)
data DecisionTaskCompletedEventAttributes = DecisionTaskCompletedEventAttributes
    { executionContext :: Data
    , scheduledEventId :: EventId
    , startedEventId :: EventId
    } deriving (Eq,Show)
newtype DescribeDomainInput = DescribeDomainInput
    { name :: DomainName
    } deriving (Eq,Show)
data ChildWorkflowExecutionFailedEventAttributes = ChildWorkflowExecutionFailedEventAttributes
    { workflowExecution :: WorkflowExecution
    , workflowType :: WorkflowType
    , reason :: FailureReason
    , details :: Data
    , initiatedEventId :: EventId
    , startedEventId :: EventId
    } deriving (Eq,Show)
data GetWorkflowExecutionHistoryInput = GetWorkflowExecutionHistoryInput
    { domain :: DomainName
    , execution :: WorkflowExecution
    , nextPageToken :: PageToken
    , maximumPageSize :: PageSize
    , reverseOrder :: ReverseOrder
    } deriving (Eq,Show)
data RecordActivityTaskHeartbeatInput = RecordActivityTaskHeartbeatInput
    { taskToken :: TaskToken
    , details :: LimitedData
    } deriving (Eq,Show)
data DomainInfo = DomainInfo
    { name :: DomainName
    , status :: RegistrationStatus
    , description :: Description
    } deriving (Eq,Show)
data HistoryEvent = HistoryEvent
    { eventTimestamp :: Timestamp
    , eventType :: EventType
    , eventId :: EventId
    , workflowExecutionStartedEventAttributes :: WorkflowExecutionStartedEventAttributes
    , workflowExecutionCompletedEventAttributes :: WorkflowExecutionCompletedEventAttributes
    , completeWorkflowExecutionFailedEventAttributes :: CompleteWorkflowExecutionFailedEventAttributes
    , workflowExecutionFailedEventAttributes :: WorkflowExecutionFailedEventAttributes
    , failWorkflowExecutionFailedEventAttributes :: FailWorkflowExecutionFailedEventAttributes
    , workflowExecutionTimedOutEventAttributes :: WorkflowExecutionTimedOutEventAttributes
    , workflowExecutionCanceledEventAttributes :: WorkflowExecutionCanceledEventAttributes
    , cancelWorkflowExecutionFailedEventAttributes :: CancelWorkflowExecutionFailedEventAttributes
    , workflowExecutionContinuedAsNewEventAttributes :: WorkflowExecutionContinuedAsNewEventAttributes
    , continueAsNewWorkflowExecutionFailedEventAttributes :: ContinueAsNewWorkflowExecutionFailedEventAttributes
    , workflowExecutionTerminatedEventAttributes :: WorkflowExecutionTerminatedEventAttributes
    , workflowExecutionCancelRequestedEventAttributes :: WorkflowExecutionCancelRequestedEventAttributes
    , decisionTaskScheduledEventAttributes :: DecisionTaskScheduledEventAttributes
    , decisionTaskStartedEventAttributes :: DecisionTaskStartedEventAttributes
    , decisionTaskCompletedEventAttributes :: DecisionTaskCompletedEventAttributes
    , decisionTaskTimedOutEventAttributes :: DecisionTaskTimedOutEventAttributes
    , activityTaskScheduledEventAttributes :: ActivityTaskScheduledEventAttributes
    , activityTaskStartedEventAttributes :: ActivityTaskStartedEventAttributes
    , activityTaskCompletedEventAttributes :: ActivityTaskCompletedEventAttributes
    , activityTaskFailedEventAttributes :: ActivityTaskFailedEventAttributes
    , activityTaskTimedOutEventAttributes :: ActivityTaskTimedOutEventAttributes
    , activityTaskCanceledEventAttributes :: ActivityTaskCanceledEventAttributes
    , activityTaskCancelRequestedEventAttributes :: ActivityTaskCancelRequestedEventAttributes
    , workflowExecutionSignaledEventAttributes :: WorkflowExecutionSignaledEventAttributes
    , markerRecordedEventAttributes :: MarkerRecordedEventAttributes
    , recordMarkerFailedEventAttributes :: RecordMarkerFailedEventAttributes
    , timerStartedEventAttributes :: TimerStartedEventAttributes
    , timerFiredEventAttributes :: TimerFiredEventAttributes
    , timerCanceledEventAttributes :: TimerCanceledEventAttributes
    , startChildWorkflowExecutionInitiatedEventAttributes :: StartChildWorkflowExecutionInitiatedEventAttributes
    , childWorkflowExecutionStartedEventAttributes :: ChildWorkflowExecutionStartedEventAttributes
    , childWorkflowExecutionCompletedEventAttributes :: ChildWorkflowExecutionCompletedEventAttributes
    , childWorkflowExecutionFailedEventAttributes :: ChildWorkflowExecutionFailedEventAttributes
    , childWorkflowExecutionTimedOutEventAttributes :: ChildWorkflowExecutionTimedOutEventAttributes
    , childWorkflowExecutionCanceledEventAttributes :: ChildWorkflowExecutionCanceledEventAttributes
    , childWorkflowExecutionTerminatedEventAttributes :: ChildWorkflowExecutionTerminatedEventAttributes
    , signalExternalWorkflowExecutionInitiatedEventAttributes :: SignalExternalWorkflowExecutionInitiatedEventAttributes
    , externalWorkflowExecutionSignaledEventAttributes :: ExternalWorkflowExecutionSignaledEventAttributes
    , signalExternalWorkflowExecutionFailedEventAttributes :: SignalExternalWorkflowExecutionFailedEventAttributes
    , externalWorkflowExecutionCancelRequestedEventAttributes :: ExternalWorkflowExecutionCancelRequestedEventAttributes
    , requestCancelExternalWorkflowExecutionInitiatedEventAttributes :: RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
    , requestCancelExternalWorkflowExecutionFailedEventAttributes :: RequestCancelExternalWorkflowExecutionFailedEventAttributes
    , scheduleActivityTaskFailedEventAttributes :: ScheduleActivityTaskFailedEventAttributes
    , requestCancelActivityTaskFailedEventAttributes :: RequestCancelActivityTaskFailedEventAttributes
    , startTimerFailedEventAttributes :: StartTimerFailedEventAttributes
    , cancelTimerFailedEventAttributes :: CancelTimerFailedEventAttributes
    , startChildWorkflowExecutionFailedEventAttributes :: StartChildWorkflowExecutionFailedEventAttributes
    } deriving (Eq,Show)
data DescribeActivityTypeInput = DescribeActivityTypeInput
    { domain :: DomainName
    , activityType :: ActivityType
    } deriving (Eq,Show)
data DeprecateActivityTypeInput = DeprecateActivityTypeInput
    { domain :: DomainName
    , activityType :: ActivityType
    } deriving (Eq,Show)
data TerminateWorkflowExecutionInput = TerminateWorkflowExecutionInput
    { domain :: DomainName
    , workflowId :: WorkflowId
    , runId :: RunIdOptional
    , reason :: TerminateReason
    , details :: Data
    , childPolicy :: ChildPolicy
    } deriving (Eq,Show)
data WorkflowTypeInfos = WorkflowTypeInfos
    { typeInfos :: WorkflowTypeInfoList
    , nextPageToken :: PageToken
    } deriving (Eq,Show)
data ContinueAsNewWorkflowExecutionFailedEventAttributes = ContinueAsNewWorkflowExecutionFailedEventAttributes
    { cause :: ContinueAsNewWorkflowExecutionFailedCause
    , decisionTaskCompletedEventId :: EventId
    } deriving (Eq,Show)
data SignalExternalWorkflowExecutionInitiatedEventAttributes = SignalExternalWorkflowExecutionInitiatedEventAttributes
    { workflowId :: WorkflowId
    , runId :: RunIdOptional
    , signalName :: SignalName
    , input :: Data
    , decisionTaskCompletedEventId :: EventId
    , control :: Data
    } deriving (Eq,Show)
newtype CancelTimerDecisionAttributes = CancelTimerDecisionAttributes
    { timerId :: TimerId
    } deriving (Eq,Show)
data WorkflowExecutionFailedEventAttributes = WorkflowExecutionFailedEventAttributes
    { reason :: FailureReason
    , details :: Data
    , decisionTaskCompletedEventId :: EventId
    } deriving (Eq,Show)
data WorkflowExecutionConfiguration = WorkflowExecutionConfiguration
    { taskStartToCloseTimeout :: DurationInSeconds
    , executionStartToCloseTimeout :: DurationInSeconds
    , taskList :: TaskList
    , taskPriority :: TaskPriority
    , childPolicy :: ChildPolicy
    } deriving (Eq,Show)
data WorkflowExecution = WorkflowExecution
    { workflowId :: WorkflowId
    , runId :: RunId
    } deriving (Eq,Show)
data SignalWorkflowExecutionInput = SignalWorkflowExecutionInput
    { domain :: DomainName
    , workflowId :: WorkflowId
    , runId :: RunIdOptional
    , signalName :: SignalName
    , input :: Data
    } deriving (Eq,Show)
data ContinueAsNewWorkflowExecutionDecisionAttributes = ContinueAsNewWorkflowExecutionDecisionAttributes
    { input :: Data
    , executionStartToCloseTimeout :: DurationInSecondsOptional
    , taskList :: TaskList
    , taskPriority :: TaskPriority
    , taskStartToCloseTimeout :: DurationInSecondsOptional
    , childPolicy :: ChildPolicy
    , tagList :: TagList
    , workflowTypeVersion :: Version
    } deriving (Eq,Show)
data ExternalWorkflowExecutionCancelRequestedEventAttributes = ExternalWorkflowExecutionCancelRequestedEventAttributes
    { workflowExecution :: WorkflowExecution
    , initiatedEventId :: EventId
    } deriving (Eq,Show)
data PendingTaskCount = PendingTaskCount
    { count :: Count
    , truncated :: Truncated
    } deriving (Eq,Show)
data DomainDetail = DomainDetail
    { domainInfo :: DomainInfo
    , configuration :: DomainConfiguration
    } deriving (Eq,Show)
data WorkflowExecutionContinuedAsNewEventAttributes = WorkflowExecutionContinuedAsNewEventAttributes
    { input :: Data
    , decisionTaskCompletedEventId :: EventId
    , newExecutionRunId :: RunId
    , executionStartToCloseTimeout :: DurationInSecondsOptional
    , taskList :: TaskList
    , taskPriority :: TaskPriority
    , taskStartToCloseTimeout :: DurationInSecondsOptional
    , childPolicy :: ChildPolicy
    , tagList :: TagList
    , workflowType :: WorkflowType
    } deriving (Eq,Show)
