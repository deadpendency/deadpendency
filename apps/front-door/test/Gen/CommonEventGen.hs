{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Gen.CommonEventGen where

import CommonTest.Gen.General
import GitHub.Data.Webhooks.Payload
import Hedgehog
import Hedgehog.Gen qualified as Gen

genHookRepository :: Gen HookRepository
genHookRepository = do
  repoId <- genPositiveInt
  repoNodeId <- genAlphaText
  repoName <- genAlphaText
  repoDependencyName <- genAlphaText
  repoOwner <- Right <$> genHookUser
  repoIsPrivate <- Gen.bool
  htmlUrl <- genUrl
  description <- genAlphaText
  isAFork <- Gen.bool
  url' <- genUrl
  forksUrl <- genUrl
  keysUrl <- genUrl
  collaboratorsUrl <- genUrl
  teamsUrl <- genUrl
  hooksUrl <- genUrl
  issueEventsUrl <- genUrl
  eventsUrl <- genUrl
  assigneesUrl <- genUrl
  branchesUrl <- genUrl
  tagsUrl <- genUrl
  blobsUrl <- genUrl
  gitTagsUrl <- genUrl
  gitRefsUrl <- genUrl
  treesUrl <- genUrl
  statusesUrl <- genUrl
  languagesUrl <- genUrl
  stargazersUrl <- genUrl
  contributorsUrl <- genUrl
  subscribersUrl <- genUrl
  subscriptionUrl <- genUrl
  commitsUrl <- genUrl
  gitCommitsUrl <- genUrl
  commentsUrl <- genUrl
  issueCommentsUrl <- genUrl
  contentsUrl <- genUrl
  compareUrl <- genUrl
  mergesUrl <- genUrl
  archiveUrl <- genUrl
  downloadsUrl <- genUrl
  issuesUrl <- genUrl
  pullsUrl <- genUrl
  milestonesUrl <- genUrl
  notificationsUrl <- genUrl
  labelsUrl <- genUrl
  releasesUrl <- genUrl
  createdAt <- genUTCTime
  updatedAt <- genUTCTime
  pushedAt <- genUTCTime
  gitUrl <- genUrl
  sshUrl <- genUrl
  cloneUrl <- genUrl
  svnUrl <- genUrl
  homepage <- Gen.maybe genUrl
  size <- genPositiveInt
  stargazersCount <- genPositiveInt
  watchersCount <- genPositiveInt
  language <- Gen.maybe genAlphaText
  hasIssues <- Gen.bool
  hasDownloads <- Gen.bool
  hasWiki <- Gen.bool
  hasPages <- Gen.bool
  forkCount <- genPositiveInt
  mirrorUrl <- Gen.maybe genUrl
  openIssuesCount <- genPositiveInt
  defaultBranchName <- genAlphaText

  pure
    HookRepository
      { whRepoId = repoId,
        whRepoNodeId = repoNodeId,
        whRepoName = repoName,
        whRepoFullName = repoDependencyName,
        whRepoOwner = repoOwner,
        whRepoIsPrivate = repoIsPrivate,
        whRepoHtmlUrl = htmlUrl,
        whRepoDescription = description,
        whRepoIsAFork = isAFork,
        whRepoUrl = url',
        whRepoForksUrl = forksUrl,
        whRepoKeysUrl = keysUrl,
        whRepoCollaboratorsUrl = collaboratorsUrl,
        whRepoTeamsUrl = teamsUrl,
        whRepoHooksUrl = hooksUrl,
        whRepoIssueEventsUrl = issueEventsUrl,
        whRepoEventsUrl = eventsUrl,
        whRepoAssigneesUrl = assigneesUrl,
        whRepoBranchesUrl = branchesUrl,
        whRepoTagsUrl = tagsUrl,
        whRepoBlobsUrl = blobsUrl,
        whRepoGitTagsUrl = gitTagsUrl,
        whRepoGitRefsUrl = gitRefsUrl,
        whRepoTreesUrl = treesUrl,
        whRepoStatusesUrl = statusesUrl,
        whRepoLanguagesUrl = languagesUrl,
        whRepoStargazersUrl = stargazersUrl,
        whRepoContributorsUrl = contributorsUrl,
        whRepoSubscribersUrl = subscribersUrl,
        whRepoSubscriptionUrl = subscriptionUrl,
        whRepoCommitsUrl = commitsUrl,
        whRepoGitCommitsUrl = gitCommitsUrl,
        whRepoCommentsUrl = commentsUrl,
        whRepoIssueCommentsUrl = issueCommentsUrl,
        whRepoContentsUrl = contentsUrl,
        whRepoCompareUrl = compareUrl,
        whRepoMergesUrl = mergesUrl,
        whRepoArchiveUrl = archiveUrl,
        whRepoDownloadsUrl = downloadsUrl,
        whRepoIssuesUrl = issuesUrl,
        whRepoPullsUrl = pullsUrl,
        whRepoMilestonesUrl = milestonesUrl,
        whRepoNotificationsUrl = notificationsUrl,
        whRepoLabelsUrl = labelsUrl,
        whRepoReleasesUrl = releasesUrl,
        whRepoCreatedAt = createdAt,
        whRepoUpdatedAt = updatedAt,
        whRepoPushedAt = pushedAt,
        whRepoGitUrl = gitUrl,
        whRepoSshUrl = sshUrl,
        whRepoCloneUrl = cloneUrl,
        whRepoSvnUrl = svnUrl,
        whRepoHomepage = homepage,
        whRepoSize = size,
        whRepoStargazersCount = stargazersCount,
        whRepoWatchersCount = watchersCount,
        whRepoLanguage = language,
        whRepoHasIssues = hasIssues,
        whRepoHasDownloads = hasDownloads,
        whRepoHasWiki = hasWiki,
        whRepoHasPages = hasPages,
        whRepoForkCount = forkCount,
        whRepoMirrorUrl = mirrorUrl,
        whRepoOpenIssuesCount = openIssuesCount,
        whRepoDefaultBranchName = defaultBranchName
      }

genHookOrganization :: Gen HookOrganization
genHookOrganization = do
  login <- genAlphaText
  id' <- genPositiveInt
  nodeId <- genAlphaText
  url <- genUrl
  reposUrl <- genUrl
  eventsUrl <- genUrl
  hooksUrl <- Gen.maybe genUrl
  issuesUrl <- Gen.maybe genUrl
  membersUrl <- genUrl
  publicMembersUrl <- genUrl
  avatarUrl <- genUrl
  description <- genAlphaText
  pure
    HookOrganization
      { whOrgLogin = login,
        whOrgId = id',
        whOrgNodeId = nodeId,
        whOrgUrl = url,
        whOrgReposUrl = reposUrl,
        whOrgEventsUrl = eventsUrl,
        whOrgHooksUrl = hooksUrl,
        whOrgIssuesUrl = issuesUrl,
        whOrgMembersUrl = membersUrl,
        whOrgPublicMembersUrl = publicMembersUrl,
        whOrgAvatarUrl = avatarUrl,
        whOrgDescription = description
      }

genHookUser :: Gen HookUser
genHookUser = do
  login <- genAlphaText
  id' <- genPositiveInt
  nodeId <- genAlphaText
  avatarUrl <- genUrl
  gravatarId <- genUrl
  url <- genUrl
  htmlUrl <- genUrl
  followersUrl <- genUrl
  followingUrl <- genUrl
  gistsUrl <- genUrl
  starredUrl <- genUrl
  subscriptionsUrl <- genUrl
  organizationsUrl <- genUrl
  reposUrl <- genUrl
  eventsUrl <- genUrl
  receivedEventsUrl <- genUrl
  type' <- genOwnerType
  isAdminOfSite <- Gen.bool
  pure
    HookUser
      { whUserLogin = login,
        whUserId = id',
        whUserNodeId = nodeId,
        whUserAvatarUrl = avatarUrl,
        whUserGravatarId = gravatarId,
        whUserUrl = url,
        whUserHtmlUrl = htmlUrl,
        whUserFollowersUrl = followersUrl,
        whUserFollowingUrl = followingUrl,
        whUserGistsUrl = gistsUrl,
        whUserStarredUrl = starredUrl,
        whUserSubscriptionsUrl = subscriptionsUrl,
        whUserOrganizationsUrl = organizationsUrl,
        whUserReposUrl = reposUrl,
        whUserEventsUrl = eventsUrl,
        whUserReceivedEventsUrl = receivedEventsUrl,
        whUserType = type',
        whUserIsAdminOfSite = isAdminOfSite
      }

genOwnerType :: Gen OwnerType
genOwnerType =
  Gen.element
    [ OwnerUser,
      OwnerOrganization,
      OwnerBot
    ]

genHookSimpleUser :: Gen HookSimpleUser
genHookSimpleUser = do
  userName <- genAlphaText
  userEmail <- genAlphaText
  userLogin <- Gen.maybe genAlphaText
  pure
    HookSimpleUser
      { whSimplUserName = userName,
        whSimplUserEmail = userEmail,
        whSimplUserLogin = userLogin
      }

genUrl :: Gen URL
genUrl = URL <$> genAlphaText
