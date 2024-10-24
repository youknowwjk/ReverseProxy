package com.autel.cloud.pile.base.domain.utils;

import org.springframework.transaction.support.TransactionSynchronization;
import org.springframework.transaction.support.TransactionSynchronizationManager;

import java.util.concurrent.Executor;

public class TransactionUtils {

    public static void afterCommitSyncExecute(Runnable runnable){
        if (TransactionSynchronizationManager.isSynchronizationActive()) {
            TransactionSynchronizationManager.registerSynchronization(new TransactionSynchronization() {
                @Override
                public void afterCommit() {
                    runnable.run();
                }
            });
        } else {
            runnable.run();
        }
    }

    /**
     * 在事务提交后异步执行
     * @param runnable
     */
    public static void afterCommitAsyncExecute(Runnable runnable, Executor queryExecuor){
        if (TransactionSynchronizationManager.isSynchronizationActive()) {
            TransactionSynchronizationManager.registerSynchronization(new TransactionSynchronization() {
                @Override
                public void afterCommit() {
                    if (queryExecuor != null) {
                        queryExecuor.execute(runnable);
                    }else{
                        AutelThreadUtils.THREAD_POOL.execute(runnable);
                    }
                }
            });
        } else {
            if (queryExecuor != null) {
                queryExecuor.execute(runnable);
            }else{
                AutelThreadUtils.THREAD_POOL.execute(runnable);
            }
        }
    }
}
