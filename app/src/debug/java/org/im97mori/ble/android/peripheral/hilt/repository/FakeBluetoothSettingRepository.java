package org.im97mori.ble.android.peripheral.hilt.repository;

import static org.im97mori.ble.android.peripheral.utils.Utils.stackLog;

import android.content.Context;

import androidx.annotation.NonNull;

import java.util.function.Consumer;

import javax.inject.Inject;

import io.reactivex.rxjava3.functions.Action;

public class FakeBluetoothSettingRepository extends BluetoothSettingRepository {

    public Action mAddBluetoothStatusConsumerAction;

    public Action mRemoveBluetoothStatusConsumerAction;

    @Inject
    public FakeBluetoothSettingRepository(@NonNull Context context) {
        super(context);
    }

    @Override
    public void addBluetoothStatusConsumer(@NonNull Consumer<Boolean> consumer) {
        if (mAddBluetoothStatusConsumerAction != null) {
            try {
                mAddBluetoothStatusConsumerAction.run();
            } catch (Throwable throwable) {
                stackLog(throwable);
            }
        }
        super.addBluetoothStatusConsumer(consumer);
    }

    @Override
    public void removeBluetoothStatusConsumer(@NonNull Consumer<Boolean> consumer) {
        if (mRemoveBluetoothStatusConsumerAction != null) {
            try {
                mRemoveBluetoothStatusConsumerAction.run();
            } catch (Throwable throwable) {
                stackLog(throwable);
            }
        }
        super.removeBluetoothStatusConsumer(consumer);
    }

}
