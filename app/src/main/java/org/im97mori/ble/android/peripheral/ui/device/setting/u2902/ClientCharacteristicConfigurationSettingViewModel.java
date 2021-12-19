package org.im97mori.ble.android.peripheral.ui.device.setting.u2902;

import static org.im97mori.ble.constants.DescriptorUUID.CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR;

import android.bluetooth.BluetoothGatt;
import android.bluetooth.BluetoothGattDescriptor;
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

import org.im97mori.ble.DescriptorData;
import org.im97mori.ble.android.peripheral.ui.device.setting.BaseDescriptorSettingViewModel;
import org.im97mori.ble.descriptor.u2902.ClientCharacteristicConfiguration;

import java.util.Objects;
import java.util.Optional;

import io.reactivex.rxjava3.core.Completable;
import io.reactivex.rxjava3.core.Single;
import io.reactivex.rxjava3.schedulers.Schedulers;

public class ClientCharacteristicConfigurationSettingViewModel extends BaseDescriptorSettingViewModel {

    public static final String KEY_PROPERTIES_TYPE = "KEY_PROPERTIES_TYPE";

    public static final int PROPERTIES_TYPE_NOTIFICATION = 0;
    public static final int PROPERTIES_TYPE_INDICATION = 1;

    private final MutableLiveData<Boolean> isErrorResponse;

    private final MutableLiveData<Boolean> properties;
    private final MutableLiveData<String> disabledProperties;
    private final MutableLiveData<String> enabledProperties;

    private final MutableLiveData<String> responseDelay;
    private final MutableLiveData<String> responseDelayError;
    private final MutableLiveData<String> responseCode;
    private final MutableLiveData<String> responseCodeError;

    private int propertyType;

    public ClientCharacteristicConfigurationSettingViewModel(@NonNull SavedStateHandle savedStateHandle) {
        isErrorResponse = savedStateHandle.getLiveData("isErrorResponse");

        properties = savedStateHandle.getLiveData("properties");
        disabledProperties = savedStateHandle.getLiveData("disabledProperties");
        enabledProperties = savedStateHandle.getLiveData("enabledProperties");

        responseDelay = savedStateHandle.getLiveData("responseDelay");
        responseDelayError = savedStateHandle.getLiveData("responseDelayError");
        responseCode = savedStateHandle.getLiveData("responseCode");
        responseCodeError = savedStateHandle.getLiveData("responseCodeError");
    }

    @NonNull
    public Completable setup(@NonNull Intent intent) {
        Completable completable;
        if (mDescriptorData == null) {
            completable = Single.just(Optional.ofNullable(intent.getStringExtra(CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR.toString())))
                    .subscribeOn(Schedulers.io())
                    .observeOn(Schedulers.io())
                    .flatMapCompletable(dataString -> {
                        if (dataString.isPresent()) {
                            try {
                                mDescriptorData = mGson.fromJson(dataString.get(), DescriptorData.class);
                            } catch (JsonSyntaxException e) {
                                e.printStackTrace();
                            }
                        }

                        if (mDescriptorData == null) {
                            mDescriptorData = new DescriptorData(CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR
                                    , BluetoothGattDescriptor.PERMISSION_READ | BluetoothGattDescriptor.PERMISSION_WRITE
                                    , BluetoothGatt.GATT_SUCCESS
                                    , 0
                                    , null);
                        }

                        if (isErrorResponse.getValue() == null) {
                            isErrorResponse.postValue(mDescriptorData.responseCode != BluetoothGatt.GATT_SUCCESS);
                        }

                        ClientCharacteristicConfiguration clientCharacteristicConfiguration;
                        if (mDescriptorData.data != null) {
                            clientCharacteristicConfiguration = new ClientCharacteristicConfiguration(mDescriptorData.data);
                        } else {
                            clientCharacteristicConfiguration = null;
                        }

                        propertyType = intent.getIntExtra(KEY_PROPERTIES_TYPE, PROPERTIES_TYPE_NOTIFICATION);
                        if (clientCharacteristicConfiguration == null) {
                            if (properties.getValue() == null) {
                                properties.postValue(false);
                                if (PROPERTIES_TYPE_NOTIFICATION == propertyType) {
                                    disabledProperties.postValue(mResourceTextSource.getIndicationsDisabledString());
                                    enabledProperties.postValue(mResourceTextSource.getIndicationsEnabledString());
                                } else {
                                    disabledProperties.postValue(mResourceTextSource.getEnabledDisabledString());
                                    enabledProperties.postValue(mResourceTextSource.getEnabledEnabledString());
                                }
                            }
                        } else {
                            if (properties.getValue() == null) {
                                if (PROPERTIES_TYPE_NOTIFICATION == propertyType) {
                                    properties.postValue(clientCharacteristicConfiguration.isPropertiesNotificationsEnabled());
                                    disabledProperties.postValue(mResourceTextSource.getIndicationsDisabledString());
                                    enabledProperties.postValue(mResourceTextSource.getIndicationsEnabledString());
                                } else {
                                    properties.postValue(clientCharacteristicConfiguration.isPropertiesIndicationsEnabled());
                                    disabledProperties.postValue(mResourceTextSource.getEnabledDisabledString());
                                    enabledProperties.postValue(mResourceTextSource.getEnabledEnabledString());
                                }
                            }
                        }

                        String text = String.valueOf(mDescriptorData.delay);
                        if (responseDelay.getValue() == null) {
                            responseDelay.postValue(text);
                            responseDelayError.postValue(mResourceTextSource.getResponseDelayErrorString(text));
                        }

                        text = String.valueOf(mDescriptorData.responseCode);
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

    public void observeProperties(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(properties).observe(owner, observer);
    }

    @MainThread
    public synchronized void updateProperties(@NonNull Boolean checked) {
        properties.setValue(checked);
    }

    public void observeDisabledProperties(@NonNull LifecycleOwner owner, @NonNull Observer<CharSequence> observer) {
        Transformations.distinctUntilChanged(disabledProperties).observe(owner, observer);
    }

    public void observeEnabledProperties(@NonNull LifecycleOwner owner, @NonNull Observer<CharSequence> observer) {
        Transformations.distinctUntilChanged(enabledProperties).observe(owner, observer);
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
                && TextUtils.isEmpty(responseDelayError.getValue())) {
            ClientCharacteristicConfiguration clientCharacteristicConfiguration;
            if (Boolean.TRUE.equals(properties.getValue())) {
                if (PROPERTIES_TYPE_NOTIFICATION == propertyType) {
                    clientCharacteristicConfiguration = new ClientCharacteristicConfiguration(BluetoothGattDescriptor.ENABLE_NOTIFICATION_VALUE);
                } else {
                    clientCharacteristicConfiguration = new ClientCharacteristicConfiguration(BluetoothGattDescriptor.ENABLE_INDICATION_VALUE);
                }
            } else {
                clientCharacteristicConfiguration = new ClientCharacteristicConfiguration(BluetoothGattDescriptor.DISABLE_NOTIFICATION_VALUE);
            }
            mDescriptorData.data = clientCharacteristicConfiguration.getBytes();
            if (Boolean.TRUE.equals(isErrorResponse.getValue())) {
                mDescriptorData.responseCode = Integer.parseInt(Objects.requireNonNull(responseCode.getValue()));
            } else {
                mDescriptorData.responseCode = BluetoothGatt.GATT_SUCCESS;
            }
            mDescriptorData.delay = Long.parseLong(Objects.requireNonNull(responseDelay.getValue()));

            intent = new Intent();
            intent.putExtra(CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR.toString(), mGson.toJson(mDescriptorData));
        } else {
            intent = null;
        }
        return Single.just(Optional.ofNullable(intent));
    }
}