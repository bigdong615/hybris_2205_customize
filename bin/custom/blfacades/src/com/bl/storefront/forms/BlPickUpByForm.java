package com.bl.storefront.forms;

import javax.validation.constraints.Email;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;

public class BlPickUpByForm {

    private String firstName;
    private String lastName;
    private String phone;
    private String email;

    @NotNull(message = "{blPickUpBy.firstName.invalid}")
    @Size(min = 1, max = 255, message = "{blPickUpBy.firstName.invalid}")
    public String getFirstName()
    {
        return firstName;
    }

    public void setFirstName(final String firstName)
    {
        this.firstName = firstName;
    }

    @NotNull(message = "{blPickUpBy.lastName.invalid}")
    @Size(min = 1, max = 255, message = "{blPickUpBy.lastName.invalid}")
    public String getLastName()
    {
        return lastName;
    }

    public void setLastName(final String lastName)
    {
        this.lastName = lastName;
    }

    public String getPhone()
    {
        return phone;
    }

    public void setPhone(final String value)
    {
        phone = value;
    }

    @Email
    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }

}
