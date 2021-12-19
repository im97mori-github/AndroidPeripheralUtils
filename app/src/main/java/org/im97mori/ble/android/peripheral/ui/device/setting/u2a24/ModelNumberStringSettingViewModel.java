package org.im97mori.ble.android.peripheral.ui.device.setting.u2a24;

import static org.im97mori.ble.constants.CharacteristicUUID.MODEL_NUMBER_STRING_CHARACTERISTIC;

import android.bluetooth.BluetoothGatt;
import android.bluetooth.BluetoothGattCharacteristic;
import android.content.Intent;
import android.text.TextUtils;

import androidx.annotation.MainThread;
import androidx.annotation.NonNull;
import androidx.lifecycle.LifecycleOwner;
import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.Observer;
import androidx.lifecycle.SavedStateHandle;
import androidx.lifecycle.Transformations;

import com.google.gson.JsonSyntaxException;

import org.im97mori.ble.CharacteristicData;
import org.im97mori.ble.android.peripheral.ui.device.setting.BaseCharacteristicViewModel;
import org.im97mori.ble.characteristic.u2a24.ModelNumberString;
import org.im97mori.ble.characteristic.u2a29.ManufacturerNameString;

import java.util.Objects;
import java.util.Optional;

import io.reactivex.rxjava3.core.Completable;
import io.reactivex.rxjava3.core.Single;
import io.reactivex.rxjava3.schedulers.Schedulers;

public class ModelNumberStringSettingViewModel extends BaseCharacteristicViewModel {

    private final MutableLiveData<Boolean> isErrorResponse;

    private final MutableLiveData<String> modelNumberString;
    private final MutableLiveData<String> modelNumberStringError;
    private final MutableLiveData<String> responseDelay;
    private final MutableLiveData<String> responseDelayError;
    private final MutableLiveData<String> responseCode;
    private final MutableLiveData<String> responseCodeError;

    public ModelNumberStringSettingViewModel(@NonNull SavedStateHandle savedStateHandle) {
        isErrorResponse = savedStateHandle.getLiveData("isErrorResponse");
        modelNumberString = savedStateHandle.getLiveData("modelNumberString");
        modelNumberStringError = savedStateHandle.getLiveData("modelNumberStringError");
        responseDelay = savedStateHandle.getLiveData("responseDelay");
        responseDelayError = savedStateHandle.getLiveData("responseDelayError");
        responseCode = savedStateHandle.getLiveData("responseCode");
        responseCodeError = savedStateHandle.getLiveData("responseCodeError");
    }

    @NonNull
    public Completable setup(@NonNull Intent intent) {
        Completable completable;
        if (mCharacteristicData == null) {
            completable = Single.just(Optional.ofNullable(intent.getStringExtra(MODEL_NUMBER_STRING_CHARACTERISTIC.toString())))
                    .subscribeOn(Schedulers.io())
                    .observeOn(Schedulers.io())
                    .flatMapCompletable(dataString -> {
                        if (dataString.isPresent()) {
                            try {
                                mCharacteristicData = mGson.fromJson(dataString.get(), CharacteristicData.class);
                            } catch (JsonSyntaxException e) {
                                e.printStackTrace();
                            }
                        }

                        if (mCharacteristicData == null) {
                            mCharacteristicData = new CharacteristicData();
                            mCharacteristicData.uuid = MODEL_NUMBER_STRING_CHARACTERISTIC;
                            mCharacteristicData.property = BluetoothGattCharacteristic.PROPERTY_READ;
                            mCharacteristicData.permission = BluetoothGattCharacteristic.PERMISSION_READ;
                        }

                        if (isErrorResponse.getValue() == null) {
                            isErrorResponse.postValue(mCharacteristicData.responseCode != BluetoothGatt.GATT_SUCCESS);
                        }

                        String text;
                        if (mCharacteristicData.data != null) {
                            text = new ModelNumberString(mCharacteristicData.data).getModelNumber();
                            if (this.modelNumberString.getValue() == null) {
                                this.modelNumberString.postValue(text);
                            }
                            modelNumberStringError.postValue(mResourceTextSource.getManufacturerNameStringErrorString(text));
                        } else {
                            modelNumberStringError.postValue(mResourceTextSource.getManufacturerNameStringErrorString(this.modelNumberString.getValue()));
                        }

                        text = String.valueOf(mCharacteristicData.delay);
                        if (responseDelay.getValue() == null) {
                            responseDelay.postValue(text);
                            responseDelayError.postValue(mResourceTextSource.getResponseDelayErrorString(text));
                        }

                        text = String.valueOf(mCharacteristicData.responseCode);
                        if (responseCode.getValue() == null) {
                            responseCode.postValue(text);
                            responseCodeError.postValue(mResourceTextSource.getResponseCodeErrorString(text));
                        }

                        return Completable.complete();
                    });
        } else {
            completable = Completable.complete();
        }
        return completable;
    }

    public void observeIsErrorResponse(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(isErrorResponse).observe(owner, observer);
    }

    @MainThread
    public synchronized void updateIsErrorResponse(@NonNull Boolean checked) {
        isErrorResponse.setValue(checked);
    }

    public void observeManufacturerNameString(@NonNull LifecycleOwner owner, @NonNull Observer<CharSequence> observer) {
        Transformations.distinctUntilChanged(modelNumberString).observe(owner, observer);
    }

    public void observeManufacturerNameStringError(@NonNull LifecycleOwner owner, @NonNull Observer<CharSequence> observer) {
        Transformations.distinctUntilChanged(modelNumberStringError).observe(owner, observer);
    }

    @MainThread
    public synchronized void updateManufacturerNameString(@NonNull CharSequence text) {
        modelNumberString.setValue(text.toString());
        modelNumberStringError.setValue(mResourceTextSource.getModelNumbertringErrorString(text));
    }

    public void observeResponseDelay(@NonNull LifecycleOwner owner, @NonNull Observer<CharSequence> observer) {
        Transformations.distinctUntilChanged(responseDelay).observe(owner, observer);
    }

    public void observeResponseDelayError(@NonNull LifecycleOwner owner, @NonNull Observer<CharSequence> observer) {
        Transformations.distinctUntilChanged(responseDelayError).observe(owner, observer);
    }

    @MainThread
    public synchronized void updateResponseDelay(@NonNull CharSequence text) {
        responseDelay.setValue(text.toString());
        responseDelayError.setValue(mResourceTextSource.getResponseDelayErrorString(text));
    }

    public void observeResponseCode(@NonNull LifecycleOwner owner, @NonNull Observer<CharSequence> observer) {
        Transformations.distinctUntilChanged(responseCode).observe(owner, observer);
    }

    public void observeResponseCodeError(@NonNull LifecycleOwner owner, @NonNull Observer<CharSequence> observer) {
        Transformations.distinctUntilChanged(responseCodeError).observe(owner, observer);
    }

    @MainThread
    public synchronized void updateResponseCode(@NonNull CharSequence text) {
        responseCode.setValue(text.toString());
        responseCodeError.setValue(mResourceTextSource.getResponseCodeErrorString(text));
    }

    @NonNull
    @Override
    public Single<Optional<Intent>> save() {
        Intent intent;
        if ((Boolean.TRUE.equals(isErrorResponse.getValue())
                && TextUtils.isEmpty(responseCodeError.getValue())
                || Boolean.FALSE.equals(isErrorResponse.getValue()))
                && TextUtils.isEmpty(modelNumberStringError.getValue())
                && TextUtils.isEmpty(responseDelayError.getValue())) {
            mCharacteristicData.data = new ManufacturerNameString(Objects.requireNonNull(modelNumberString.getValue())).getBytes();
            if (Boolean.TRUE.equals(isErrorResponse.getValue())) {
                mCharacteristicData.responseCode = Integer.parseInt(Objects.requireNonNull(responseCode.getValue()));
            } else {
                mCharacteristicData.responseCode = BluetoothGatt.GATT_SUCCESS;
            }
            mCharacteristicData.delay = Long.parseLong(Objects.requireNonNull(responseDelay.getValue()));

            intent = new Intent();
            intent.putExtra(MODEL_NUMBER_STRING_CHARACTERISTIC.toString(), mGson.toJson(mCharacteristicData));
        } else {
            intent = null;
        }
        return Single.just(Optional.ofNullable(intent));
    }
}